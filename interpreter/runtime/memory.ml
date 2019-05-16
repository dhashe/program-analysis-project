open Bigarray
open Lib.Bigarray
open Types
open Values

type size = int32  (* number of pages *)
type address = int64
type offset = int32

type pack_size = Pack8 | Pack16 | Pack32
type extension = SX | ZX

(* TODO Use Z3 array logic to implement the array. *)
(* Start with a fully concrete BigArray, but keep track of the written bytes *)
(* When we receive a symbolic expression, then create the parallel Z3 array and initialize it with the written bytes *)

type memory' = (int, int8_unsigned_elt, c_layout) Array1.t
type memory = {mutable content : memory'; max : size option;
               mutable log : (address, bool) Hashtbl.t;
               mutable symbolic_content : Z3.Expr.expr option}
type t = memory

exception Type
exception Bounds
exception SizeOverflow
exception SizeLimit
exception OutOfMemory

let page_size = 0x10000L (* 64 KiB *)

let packed_size = function
  | Pack8 -> 1
  | Pack16 -> 2
  | Pack32 -> 4

let within_limits n = function
  | None -> true
  | Some max -> Concreteness.was_concrete (I32.le_u n max)

let create n =
  if Concreteness.was_concrete (I32.gt_u n (I32.of_bits 0x10000l)) then raise SizeOverflow else
  try
    let size = Int64.(mul (of_int32 (I32.to_bits n)) page_size) in
    let mem = Array1_64.create Int8_unsigned C_layout size in
    Array1.fill mem 0;
    mem
  with Out_of_memory -> raise OutOfMemory

let alloc (MemoryType {min; max}) =
  let mint = I32.of_bits min in
  let maxt = match max with None -> None | Some v -> Some (I32.of_bits v) in
  assert (within_limits mint maxt);
  {content = create mint; max; log = Hashtbl.create 10; symbolic_content = None}

let bound mem =
  Array1_64.dim mem.content

let size mem =
  Int64.(to_int32 (div (bound mem) page_size))

let type_of mem =
  MemoryType {min = size mem; max = mem.max}

let grow mem delta =
  let old_size = size mem in
  let new_size = Int32.add old_size delta in
  if Concreteness.was_concrete (I32.gt_u (I32.of_bits old_size) (I32.of_bits new_size)) then raise SizeOverflow else
  if not (within_limits (I32.of_bits new_size) (match mem.max with None -> None | Some v -> Some (I32.of_bits v))) then raise SizeLimit else
  let after = create (I32.of_bits new_size) in
  let dim = Array1_64.dim mem.content in
  Array1.blit (Array1_64.sub mem.content 0L dim) (Array1_64.sub after 0L dim);
  mem.content <- after

let load_byte mem a =
  match mem.symbolic_content with
  | Some sym_log -> failwith "TODO Implement sym_log"
  | None -> (try Array1_64.get mem.content a with Invalid_argument _ -> raise Bounds)

let store_byte mem a b =
  match mem.symbolic_content with
  | Some sym_log -> failwith "TODO Implement sym_log"
  | None -> (try Array1_64.set mem.content a b with Invalid_argument _ -> raise Bounds)

let load_bytes mem a n =
  let buf = Buffer.create n in
  for i = 0 to n - 1 do
    Buffer.add_char buf (Char.chr (load_byte mem Int64.(add a (of_int i))))
  done;
  Buffer.contents buf

let store_bytes mem a bs =
  for i = String.length bs - 1 downto 0 do
    store_byte mem Int64.(add a (of_int i)) (Char.code bs.[i])
  done

let effective_address a o =
  let ea = Int64.(add a (of_int32 o)) in
  if Concreteness.was_concrete (I64.lt_u (I64.of_bits ea) (I64.of_bits a)) then raise Bounds;
  ea

let loadn mem a o n =
  assert (n > 0 && n <= 8);
  let rec loop a n =
    if n = 0 then 0L else begin
      let x = Int64.(shift_left (loop (add a 1L) (n - 1)) 8) in
      Int64.logor (Int64.of_int (load_byte mem a)) x
    end
  in loop (effective_address a o) n

let storen mem a o n x =
  assert (n > 0 && n <= 8);
  let rec loop a n x =
    if n > 0 then begin
      Int64.(loop (add a 1L) (n - 1) (shift_right x 8));
      store_byte mem a (Int64.to_int x land 0xff)
    end
  in loop (effective_address a o) n x

let load_value (mem : memory) (a : address) (o : offset) (t : Types.value_type) : Values.value =
  let n = loadn mem a o (Types.size t) in
  match t with
  | I32Type -> I32 (I32.of_bits (Int64.to_int32 n))
  | I64Type -> I64 (I64.of_bits n)
  | F32Type -> F32 (F32.of_bits (Int64.to_int32 n))
  | F64Type -> F64 (F64.of_bits n)

let store_value (mem : memory) (a : address) (o : offset) (v : Values.value) : unit =
  let x =
    match v with
    | I32 x -> Int64.of_int32 (I32.to_bits x)
    | I64 x -> I64.to_bits x
    | F32 x -> Int64.of_int32 (F32.to_bits x)
    | F64 x -> F64.to_bits x
  in storen mem a o (Types.size (Values.type_of v)) x

let extend x n = function
  | ZX -> x
  | SX -> let sh = 64 - 8 * n in Int64.(shift_right (shift_left x sh) sh)

let load_packed (sz : pack_size) (ext : extension) (mem : memory) (a : address) (o : offset) (t : Types.value_type) : Values.value =
  assert (packed_size sz <= Types.size t);
  let n = packed_size sz in
  let x = extend (loadn mem a o n) n ext in
  match t with
  | I32Type -> I32 (I32.of_bits (Int64.to_int32 x))
  | I64Type -> I64 (I64.of_bits x)
  | _ -> raise Type

let store_packed (sz : pack_size) (mem : memory) (a : address) (o : offset) (v : Values.value) : unit =
  assert (packed_size sz <= Types.size (Values.type_of v));
  let n = packed_size sz in
  let x =
    match v with
    | I32 x -> Int64.of_int32 (I32.to_bits x)
    | I64 x -> I64.to_bits x
    | _ -> raise Type
  in storen mem a o n x
