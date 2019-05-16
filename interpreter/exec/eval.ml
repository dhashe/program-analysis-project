open Values
open Types
open Instance
open Ast
open Source


(* Errors *)

module Link = Error.Make ()
module Trap = Error.Make ()
module Crash = Error.Make ()
module Exhaustion = Error.Make ()

exception Link = Link.Error
exception Trap = Trap.Error
exception Crash = Crash.Error (* failure that cannot happen in valid code *)
exception Exhaustion = Exhaustion.Error

(* this path leads to an error, report the location and a satisfying set of inputs *)
(* module SymError =
 * struct
 *   exception Error of Source.region * string * Z3.Expr.expr
 * 
 *   (\* TODO Use and uncomment *\)
 *   (\* let error at m pc = raise (Error (at, m, pc)) *\)
 * end *)
(* exception Sym_error = SymError.Error *)

let valOf = function Some x -> x | None -> failwith "valOf None"

let memory_error at = function
  | Memory.Bounds -> "out of bounds memory access"
  | Memory.SizeOverflow -> "memory size overflow"
  | Memory.SizeLimit -> "memory size limit reached"
  | Memory.Type -> Crash.error at "type mismatch at memory access"
  | exn -> raise exn

let numeric_error at = function
  | Numeric_error.IntegerOverflow -> "integer overflow"
  | Numeric_error.IntegerDivideByZero -> "integer divide by zero"
  | Numeric_error.InvalidConversionToInteger -> "invalid conversion to integer"
  | Eval_numeric.TypeError (i, v, t) ->
    Crash.error at
      ("type error, expected " ^ Types.string_of_value_type t ^ " as operand " ^
       string_of_int i ^ ", got " ^ Types.string_of_value_type (type_of v))
  | exn -> raise exn


(* Administrative Expressions & Configurations *)

type 'a stack = 'a list

type frame =
{
  inst : module_inst;
  locals : value ref list;
}

type code = value stack * admin_instr list

and admin_instr = admin_instr' phrase
and admin_instr' =
  | Plain of instr'
  | Invoke of func_inst
  | Trapping of string
  | Returning of value stack
  | Breaking of int32 * value stack
  | Label of int * instr list * code
  | Frame of int * frame * code

type config =
{
  frame : frame;
  code : code;
  budget : int;  (* to model stack overflow *)
}

let copy_config c =
  if c.frame.inst.memories <> [] then failwith "Symbolic: Don't know how to copy memory" else
    if c.frame.inst.tables <> [] then failwith "Symbolic: Don't know how to copy tables" else
      let globals' = List.map (fun g -> Global.alloc (Global.type_of g) (Global.load g)) c.frame.inst.globals in
      let locals' = List.map (fun lr -> ref (!lr)) c.frame.locals in
      let frame' = {inst = {c.frame.inst with globals = globals'}; locals = locals'} in
      {c with frame = frame'}

let frame inst locals = {inst; locals}
let config inst vs es = {frame = frame inst []; code = vs, es; budget = 300}

let plain e = Plain e.it @@ e.at

let lookup category list x =
  try Lib.List32.nth list x.it with Failure _ ->
    Crash.error x.at ("undefined " ^ category ^ " " ^ Int32.to_string x.it)

let type_ (inst : module_inst) x = lookup "type" inst.types x
let func (inst : module_inst) x = lookup "function" inst.funcs x
let table (inst : module_inst) x = lookup "table" inst.tables x
let memory (inst : module_inst) x = lookup "memory" inst.memories x
let global (inst : module_inst) x = lookup "global" inst.globals x
let local (frame : frame) x = lookup "local" frame.locals x

let elem inst x i at =
  match Table.load (table inst x) i with
  | Table.Uninitialized ->
    Trap.error at ("uninitialized element " ^ Int32.to_string i)
  | f -> f
  | exception Table.Bounds ->
    Trap.error at ("undefined element " ^ Int32.to_string i)

let func_elem inst x i at =
  match elem inst x i at with
  | FuncElem f -> f
  | _ -> Crash.error at ("type mismatch for element " ^ Int32.to_string i)

let take n (vs : 'a stack) at =
  try Lib.List.take n vs with Failure _ -> Crash.error at "stack underflow"

let drop n (vs : 'a stack) at =
  try Lib.List.drop n vs with Failure _ -> Crash.error at "stack underflow"


(* Evaluation *)

(*
 * Conventions:
 *   e  : instr
 *   v  : value
 *   es : instr list
 *   vs : value stack
 *   c : config
 *)

let rec step (path_idx : int option) (solver : Z3.Solver.solver option) (c : config) : config list * Z3.Solver.solver list option =
  let {frame; code = vs, es; _} = c in
  let solver' = match solver with Some s -> Some [s] | None -> None in
  let e = List.hd es in
  (* let vs', es' = *)
    match e.it, vs with
    | Plain e', vs ->
      (match e', vs with
      | Unreachable, vs ->
        ([{c with code = vs, [Trapping "unreachable executed" @@ e.at] @ List.tl es}], solver')

      | Nop, vs ->
        ([{c with code = vs, [] @ List.tl es}], solver')

      | Block (ts, es'), vs ->
        ([{c with code = vs, [Label (List.length ts, [], ([], List.map plain es')) @@ e.at] @ List.tl es}], solver')

      | Loop (ts, use_idx, es'), vs ->
        ([{c with code = vs, [Label (0, [(Loop (ts, valOf path_idx, es')) @@ e.at], ([], List.map plain es')) @@ e.at] @ List.tl es}], solver')

      | If (ts, es1, es2), I32 i :: vs' when i = I32.zero ->
        ([{c with code = vs', [Plain (Block (ts, es2)) @@ e.at] @ List.tl es}], solver')

      | If (ts, es1, es2), I32 (Concreteness.Concrete i) :: vs' ->
        ([{c with code = vs', [Plain (Block (ts, es1)) @@ e.at] @ List.tl es}], solver')

       (* DONE: If then else *)
      | If (ts, es1, es2), I32 (Concreteness.Symbolic i) :: vs' -> (
          print_string "if then else\n";
          let if_possible = Concreteness.try_constraints
                [Z3.Boolean.mk_not Concreteness.ctx (Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32))] in
          let else_possible = Concreteness.try_constraints
              [Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32)] in
          match (if_possible, else_possible) with
            (Some if_mdl, Some else_mdl) ->
            let if_config = {(copy_config c) with code = vs', [Plain (Block (ts, es1)) @@ e.at] @ List.tl es} in
            let else_config = {c with code = vs', [Plain (Block (ts, es2)) @@ e.at] @ List.tl es} in
            let else_solver = valOf solver in
            let if_solver = Z3.Solver.translate else_solver Concreteness.ctx in
            Z3.Solver.add if_solver
              [Z3.Boolean.mk_not Concreteness.ctx (Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32))];
            Z3.Solver.add else_solver
              [Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32)];
            ([if_config; else_config], Some [if_solver; else_solver])

          | (Some if_mdl, None) ->
            let if_config = {c with code = vs', [Plain (Block (ts, es1)) @@ e.at] @ List.tl es} in
            let if_solver = valOf solver in
            Z3.Solver.add if_solver
              [Z3.Boolean.mk_not Concreteness.ctx (Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32))];
            ([if_config], Some [if_solver])
          | (None, Some else_mdl) ->
            let else_config = {c with code = vs', [Plain (Block (ts, es2)) @@ e.at] @ List.tl es} in
            let else_solver = valOf solver in
            Z3.Solver.add else_solver
              [Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32)];
            ([else_config], Some [else_solver])
          | (None, None) ->
            ([], Some [])
          )

       (* Unconditional branch *)
      | Br x, vs ->
        ([{c with code = [], [Breaking (x.it, vs) @@ e.at] @ List.tl es}], solver')

      | BrIf x, I32 i :: vs' when i = I32.zero ->
        ([{c with code = vs', [] @ List.tl es}], solver')

      | BrIf x, I32 (Concreteness.Concrete i) :: vs' ->
        ([{c with code = vs', [Plain (Br x) @@ e.at] @ List.tl es}], solver')

      (* DONE: Conditional *)
      | BrIf x, I32 (Concreteness.Symbolic i) :: vs' -> (
          let yes_possible = Concreteness.try_constraints
                [Z3.Boolean.mk_not Concreteness.ctx (Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32))] in
          let no_possible = Concreteness.try_constraints
              [Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32)] in
          match (yes_possible, no_possible) with
            (Some yes_mdl, Some no_mdl) ->
            let no_config = {(copy_config c) with code = vs', [] @ List.tl es} in
            let yes_config = {c with code = vs', [Plain (Br x) @@ e.at] @ List.tl es} in
            let no_solver = valOf solver in
            let yes_solver = Z3.Solver.translate no_solver Concreteness.ctx in
            Z3.Solver.add yes_solver
              [Z3.Boolean.mk_not Concreteness.ctx (Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32))];
            Z3.Solver.add no_solver
              [Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32)];
            ([no_config; yes_config], Some [no_solver; yes_solver])

          | (Some yes_mdl, None) ->
            let yes_solver = valOf solver in
            let yes_config = {c with code = vs', [Plain (Br x) @@ e.at] @ List.tl es} in
            Z3.Solver.add yes_solver
              [Z3.Boolean.mk_not Concreteness.ctx (Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32))];
            ([yes_config], Some [yes_solver])

          | (None, Some no_mdl) ->
            let no_solver = valOf solver in
            let no_config = {(copy_config c) with code = vs', [] @ List.tl es} in
            Z3.Solver.add no_solver
              [Z3.Boolean.mk_eq Concreteness.ctx i (Z3.BitVector.mk_numeral Concreteness.ctx "0" 32)];
            ([no_config], Some [no_solver])

          | (None, None) ->
            ([], Some [])
          )

      (* TODO: Conditional *)
      | BrTable (xs, x), I32 i :: vs' when Concreteness.was_concrete (I32.ge_u i (I32.of_bits (Lib.List32.length xs))) ->
        ([{c with code = vs', [Plain (Br x) @@ e.at] @ List.tl es}], solver')

      | BrTable (xs, x), I32 i :: vs' ->
        ([{c with code = vs', [Plain (Br (Lib.List32.nth xs (I32.to_bits i))) @@ e.at] @ List.tl es}], solver')

       (* Unconditional branch *)
      | Return, vs ->
        ([{c with code = vs, [Returning vs @@ e.at] @ List.tl es}], solver')

      | Call x, vs ->
        ([{c with code = vs, [Invoke (func frame.inst x) @@ e.at] @ List.tl es}], solver')

      | CallIndirect x, I32 i :: vs ->
        let func = func_elem frame.inst (0l @@ e.at) (I32.to_bits i) e.at in
        if type_ frame.inst x <> Func.type_of func then
          ([{c with code = vs, [Trapping "indirect call type mismatch" @@ e.at] @ List.tl es}], solver')
        else
          ([{c with code = vs, [Invoke func @@ e.at] @ List.tl es}], solver')

      | Drop, v :: vs' ->
        ([{c with code = vs', [] @ List.tl es}], solver')

      (* TODO Branching, need to handle symbolic cases *)
      | Select, I32 i :: v2 :: v1 :: vs' when i = I32.zero ->
        ([{c with code = v2 :: vs', [] @ List.tl es}], solver')

      | Select, I32 i :: v2 :: v1 :: vs' ->
        ([{c with code = v1 :: vs', [] @ List.tl es}], solver')

      | LocalGet x, vs ->
        ([{c with code = !(local frame x) :: vs, [] @ List.tl es}], solver')

      | LocalSet x, v :: vs' ->
        local frame x := v;
        ([{c with code = vs', [] @ List.tl es}], solver')

      | LocalTee x, v :: vs' ->
        local frame x := v;
        ([{c with code = v :: vs', [] @ List.tl es}], solver')

      | GlobalGet x, vs ->
        ([{c with code = Global.load (global frame.inst x) :: vs, [] @ List.tl es}], solver')

      | GlobalSet x, v :: vs' ->
        (try Global.store (global frame.inst x) v; ([{c with code = vs', [] @ List.tl es}], solver')
        with Global.NotMutable -> Crash.error e.at "write to immutable global"
           | Global.Type -> Crash.error e.at "type mismatch at global write")

      | Load {offset; ty; sz; _}, I32 i :: vs' ->
        let mem = memory frame.inst (0l @@ e.at) in
        let addr = I64_convert.extend_i32_u i in
        (try
          let v =
            match sz with
            | None -> Memory.load_value mem (I64.to_bits addr) offset ty
            | Some (sz, ext) -> Memory.load_packed sz ext mem (I64.to_bits addr) offset ty
          in ([{c with code = v :: vs', [] @ List.tl es}], solver')
        with exn -> ([{c with code = vs', [Trapping (memory_error e.at exn) @@ e.at] @ List.tl es}], solver'))

      | Store {offset; sz; _}, v :: I32 i :: vs' ->
        let mem = memory frame.inst (0l @@ e.at) in
        let addr = I64_convert.extend_i32_u i in
        (try
          (match sz with
          | None -> Memory.store_value mem (I64.to_bits addr) offset v
          | Some sz -> Memory.store_packed sz mem (I64.to_bits addr) offset v
          );
          ([{c with code = vs', [] @ List.tl es}], solver')
        with exn -> ([{c with code = vs', [Trapping (memory_error e.at exn) @@ e.at] @ List.tl es}], solver'));

      | MemorySize, vs ->
        let mem = memory frame.inst (0l @@ e.at) in
        ([{c with code = I32 (I32.of_bits (Memory.size mem)) :: vs, [] @ List.tl es}], solver')

      | MemoryGrow, I32 delta :: vs' ->
        let mem = memory frame.inst (0l @@ e.at) in
        let old_size = Memory.size mem in
        let result =
          try Memory.grow mem (I32.to_bits delta); old_size
          with Memory.SizeOverflow | Memory.SizeLimit | Memory.OutOfMemory -> -1l
        in ([{c with code = I32 (I32.of_bits result) :: vs', [] @ List.tl es}], solver')

      | Const v, vs ->
        ([{c with code = v.it :: vs, [] @ List.tl es}], solver')

      | Test testop, v :: vs' ->
        (try ([{c with code = value_of_bool_concreteness (Eval_numeric.eval_testop testop v) :: vs', [] @ List.tl es}], solver')
         with exn -> ([{c with code = vs', [Trapping (numeric_error e.at exn) @@ e.at] @ List.tl es}], solver'))

      | Compare relop, v2 :: v1 :: vs' ->
        (try ([{c with code = value_of_bool_concreteness (Eval_numeric.eval_relop relop v1 v2) :: vs', [] @ List.tl es}], solver')
        with exn -> ([{c with code = vs', [Trapping (numeric_error e.at exn) @@ e.at] @ List.tl es}], solver'))

      | Unary unop, v :: vs' ->
        (try ([{c with code = Eval_numeric.eval_unop unop v :: vs', [] @ List.tl es}], solver')
        with exn -> ([{c with code = vs', [Trapping (numeric_error e.at exn) @@ e.at] @ List.tl es}], solver'))

      | Binary binop, v2 :: v1 :: vs' ->
        (try ([{c with code = Eval_numeric.eval_binop binop v1 v2 :: vs', [] @ List.tl es}], solver')
        with exn -> ([{c with code = vs', [Trapping (numeric_error e.at exn) @@ e.at] @ List.tl es}], solver'))

      | Convert cvtop, v :: vs' ->
        (try ([{c with code = Eval_numeric.eval_cvtop cvtop v :: vs', [] @ List.tl es}], solver')
        with exn -> ([{c with code = vs', [Trapping (numeric_error e.at exn) @@ e.at] @ List.tl es}], solver'))

      | _ ->
        let s1 = string_of_values (List.rev vs) in
        let s2 = string_of_value_types (List.map type_of (List.rev vs)) in
        Crash.error e.at
          ("missing or ill-typed operand on stack (" ^ s1 ^ " : " ^ s2 ^ ")")
      )

    | Trapping msg, vs ->
      assert false

    | Returning vs', vs ->
      Crash.error e.at "undefined frame"

    | Breaking (k, vs'), vs ->
      Crash.error e.at "undefined label"

    | Label (n, es0, (vs', [])), vs ->
      ([{c with code = vs' @ vs, [] @ List.tl es}], solver')

    | Label (n, es0, (vs', {it = Trapping msg; at} :: es')), vs ->
      ([{c with code = vs, [Trapping msg @@ at] @ List.tl es}], solver')

    | Label (n, [{it = Loop (_, use_idx, _); at = _}], (vs', {it = Returning vs0; at} :: es')), vs when (Some use_idx) = path_idx ->
      (* We've already taken this branch, so we won't take it again *)
      ([], Some [])

    | Label (n, es0, (vs', {it = Breaking (0l, vs0); at} :: es')), vs ->
      ([{c with code = take n vs0 e.at @ vs, List.map plain es0 @ List.tl es}], solver')

    | Label (n, es0, (vs', {it = Breaking (k, vs0); at} :: es')), vs ->
      ([{c with code = vs, [Breaking (Int32.sub k 1l, vs0) @@ at] @ List.tl es}], solver')

    | Label (n, es0, code'), vs ->
      let (cs', ss') = step path_idx solver {c with code = code'} in
      let configs = List.map (fun c' -> {c with code = vs, [Label (n, es0, c'.code) @@ e.at] @ List.tl es}) cs' in
      (configs, ss')

    | Frame (n, frame', (vs', [])), vs ->
      ([{c with code = vs' @ vs, [] @ List.tl es}], solver')

    | Frame (n, frame', (vs', {it = Trapping msg; at} :: es')), vs ->
      ([{c with code = vs, [Trapping msg @@ at] @ List.tl es}], solver')

    | Frame (n, frame', (vs', {it = Returning vs0; at} :: es')), vs ->
      ([{c with code = take n vs0 e.at @ vs, [] @ List.tl es}], solver')

    | Frame (n, frame', code'), vs ->
      let (cs', ss') = step path_idx solver {frame = frame'; code = code'; budget = c.budget - 1} in
      let configs = List.map (fun c' -> {c with code = vs, [Frame (n, c'.frame, c'.code) @@ e.at] @ List.tl es}) cs' in
      (configs, ss')

    | Invoke func, vs when c.budget = 0 ->
      Exhaustion.error e.at "call stack exhausted"

       (* DONE Branching *)
    | Invoke func, vs ->
      let FuncType (ins, out) = Func.type_of func in
      let n = List.length ins in
      let args, vs' = take n vs e.at, drop n vs e.at in
      (match func with
      | Func.AstFunc (t, inst', f) ->
        let locals' = List.rev args @ List.map default_value f.it.locals in
        let code' = [], [Plain (Block (out, f.it.body)) @@ f.at] in
        let frame' = {inst = !inst'; locals = List.map ref locals'} in
        ([{c with code = vs', [Frame (List.length out, frame', code') @@ e.at] @ List.tl es}], solver')

      | Func.HostFunc (t, f) ->
        try ([{c with code = List.rev (f (List.rev args)) @ vs', [] @ List.tl es}], solver')
        with Crash (_, msg) -> Crash.error e.at msg
      )
  (* in {c with code = vs', es' @ List.tl es} *)


let rec eval (c : config) : value stack =
  match c.code with
  | vs, [] ->
    vs

  | vs, {it = Trapping msg; at} :: _ ->
    Trap.error at msg

  | vs, es ->
    let (cs, ss) = step None None c in
    eval (List.hd cs)


let rec sym_eval (cs : config list) (ss : Z3.Solver.solver list) (acc : (Z3.Model.model * string) list) : (Z3.Model.model * string) list =
  print_string ("cs: " ^ (string_of_int (List.length cs)) ^ "\n");
  print_string ("ss: " ^ (String.concat "||" (List.map (fun s -> String.concat "  " (List.map Z3.Expr.to_string (Z3.Solver.get_assertions s))) ss)));
  print_string "\n";
  match cs with
    [] -> acc
  | c::cs' -> (
      match c.code with
        vs, [] -> print_string "ran a path to completion\n";
        sym_eval cs' (List.tl ss) acc

      | vs, {it = Trapping msg; at} :: _ ->
        print_string "Assertions below:\n";
        print_string ((String.concat "    " (List.map Z3.Expr.to_string (Z3.Solver.get_assertions (List.hd ss)))) ^ "\n");
        let _ = Z3.Solver.check (List.hd ss) [] in
        let mdl = valOf (Z3.Solver.get_model (List.hd ss)) in
        sym_eval cs' (List.tl ss) ((mdl, msg)::acc)

      | vs, es ->
        let (child_cs, child_ss) = step (Some (List.length cs)) (Some (List.hd ss)) c in
        sym_eval (child_cs @ cs') (valOf child_ss @ (List.tl ss)) acc
    )


(* Functions & Constants *)

let invoke (func : func_inst) (vs : value list) : value list =
  (* NOTE Validate that all elements of vs are concrete *)
  let at = match func with Func.AstFunc (_,_, f) -> f.at | _ -> no_region in
  if List.exists (function (I32 Concreteness.Symbolic _) -> true
                         | (I64 Concreteness.Symbolic _) -> true
                         | _ -> false) vs
  then Crash.error at "Symbolic values passed to invoke" else
  let FuncType (ins, out) = Func.type_of func in
  if List.map Values.type_of vs <> ins then
    Crash.error at "wrong number or types of arguments";
  let c = config empty_module_inst (List.rev vs) [Invoke func @@ at] in
  try List.rev (eval c) with Stack_overflow ->
    Exhaustion.error at "call stack exhausted"

let symbolic_invoke (func : func_inst) (vs : value list) : value list =
  (* NOTE Any and all elements of vs may be symbolic *)
  let at = match func with Func.AstFunc (_,_, f) -> f.at | _ -> no_region in
  let FuncType (ins, out) = Func.type_of func in
  if List.map Values.type_of vs <> ins then
    Crash.error at "wrong number or types of arguments";
  let cs = [config empty_module_inst (List.rev vs) [Invoke func @@ at]] in
  let ss = [Z3.Solver.mk_solver Concreteness.ctx None] in
  (* (\* Have to reset the (global, singleton, imperative) solver from last time *\)
   * let () = Z3.Solver.reset Concreteness.solver in *)
  let errors = sym_eval cs ss [] in
  List.iter (fun (mdl, msg) -> print_string ("With input model {" ^ Z3.Model.to_string mdl ^ "} we get the error: " ^ msg ^ "\n")) errors;
  []
(* TODO Handle Stack_overflow somewhere else *)
  (* try  with
   *   Stack_overflow -> Exhaustion.error at "call stack exhausted" *)

let eval_const (inst : module_inst) (const : const) : value =
  let c = config inst [] (List.map plain const.it) in
  match eval c with
  | [v] -> v
  | vs -> Crash.error const.at "wrong number of results on stack"

let i32 (v : value) at =
  match v with
  | I32 i -> i
  | _ -> Crash.error at "type error: i32 value expected"


(* Modules *)

let create_func (inst : module_inst) (f : func) : func_inst =
  Func.alloc (type_ inst f.it.ftype) (ref inst) f

let create_table (inst : module_inst) (tab : table) : table_inst =
  let {ttype} = tab.it in
  Table.alloc ttype

let create_memory (inst : module_inst) (mem : memory) : memory_inst =
  let {mtype} = mem.it in
  Memory.alloc mtype

let create_global (inst : module_inst) (glob : global) : global_inst =
  let {gtype; value} = glob.it in
  let v = eval_const inst value in
  Global.alloc gtype v

let create_export (inst : module_inst) (ex : export) : export_inst =
  let {name; edesc} = ex.it in
  let ext =
    match edesc.it with
    | FuncExport x -> ExternFunc (func inst x)
    | TableExport x -> ExternTable (table inst x)
    | MemoryExport x -> ExternMemory (memory inst x)
    | GlobalExport x -> ExternGlobal (global inst x)
  in name, ext


let init_func (inst : module_inst) (func : func_inst) =
  match func with
  | Func.AstFunc (_, inst_ref, _) -> inst_ref := inst
  | _ -> assert false

let init_table (inst : module_inst) (seg : table_segment) =
  let {index; offset = const; init} = seg.it in
  let tab = table inst index in
  let offset = i32 (eval_const inst const) const.at in
  let end_ = Int32.(add (I32.to_bits offset) (of_int (List.length init))) in
  let bound = Table.size tab in
  if Concreteness.was_concrete (I32.lt_u (I32.of_bits bound) (I32.of_bits end_))
  || Concreteness.was_concrete (I32.lt_u (I32.of_bits end_) offset) then
    Link.error seg.at "elements segment does not fit table";
  fun () ->
    Table.blit tab (I32.to_bits offset) (List.map (fun x -> FuncElem (func inst x)) init)

let init_memory (inst : module_inst) (seg : memory_segment) =
  let {index; offset = const; init} = seg.it in
  let mem = memory inst index in
  let offset' = i32 (eval_const inst const) const.at in
  let offset = I64_convert.extend_i32_u offset' in
  let end_ = Int64.(add (I64.to_bits offset) (of_int (String.length init))) in
  let bound = Memory.bound mem in
  if Concreteness.was_concrete (I64.lt_u (I64.of_bits bound) (I64.of_bits end_))
  || Concreteness.was_concrete (I64.lt_u (I64.of_bits end_) offset) then
    Link.error seg.at "data segment does not fit memory";
  fun () -> Memory.store_bytes mem (I64.to_bits offset) init


let add_import (m : module_) (ext : extern) (im : import) (inst : module_inst)
  : module_inst =
  if not (match_extern_type (extern_type_of ext) (import_type m im)) then
    Link.error im.at "incompatible import type";
  match ext with
  | ExternFunc func -> {inst with funcs = func :: inst.funcs}
  | ExternTable tab -> {inst with tables = tab :: inst.tables}
  | ExternMemory mem -> {inst with memories = mem :: inst.memories}
  | ExternGlobal glob -> {inst with globals = glob :: inst.globals}

let init (m : module_) (exts : extern list) : module_inst =
  let
    { imports; tables; memories; globals; funcs; types;
      exports; elems; data; start
    } = m.it
  in
  if List.length exts <> List.length imports then
    Link.error m.at "wrong number of imports provided for initialisation";
  let inst0 =
    { (List.fold_right2 (add_import m) exts imports empty_module_inst) with
      types = List.map (fun type_ -> type_.it) types }
  in
  let fs = List.map (create_func inst0) funcs in
  let inst1 =
    { inst0 with
      funcs = inst0.funcs @ fs;
      tables = inst0.tables @ List.map (create_table inst0) tables;
      memories = inst0.memories @ List.map (create_memory inst0) memories;
      globals = inst0.globals @ List.map (create_global inst0) globals;
    }
  in
  let inst = {inst1 with exports = List.map (create_export inst1) exports} in
  List.iter (init_func inst) fs;
  let init_elems = List.map (init_table inst) elems in
  let init_datas = List.map (init_memory inst) data in
  List.iter (fun f -> f ()) init_elems;
  List.iter (fun f -> f ()) init_datas;
  Lib.Option.app (fun x -> ignore (invoke (func inst x) [])) start;
  inst
