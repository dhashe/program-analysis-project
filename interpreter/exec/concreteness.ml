type 'xXX concreteness = Concrete of 'xXX | Symbolic of Z3.Expr.expr

let was_concrete x = match x with
    Concrete y -> y
  | Symbolic _ -> failwith "Was not concrete"

let ctx = Z3.mk_context []

let solver = Z3.Solver.mk_solver ctx None

let try_constraints constraints =
  (* let () = print_string "Hello! Trying some constraints\n" in
   * let () = print_string ((String.concat "    " (List.map Z3.Expr.to_string constraints)) ^ "\n") in
   * let () = print_string ((String.concat "    " (List.map Z3.Expr.to_string (Z3.Solver.get_assertions solver))) ^ "\n") in *)
  let () = Z3.Solver.push solver in
  let () = Z3.Solver.add solver constraints in
  let res = Z3.Solver.check solver [] in
  (* let () = print_string "Before getting model" in *)
  let model = if res = Z3.Solver.SATISFIABLE then Z3.Solver.get_model solver else None in
  (* let () = Z3.Solver.pop solver 1 in *)
  (* let () = print_string "Yay! Did the constraints" in *)
  model

let fp_round = Z3.FloatingPoint.RoundingMode.mk_rne ctx
let fp_sort32 = Z3.FloatingPoint.mk_sort_32 ctx
let fp_sort64 = Z3.FloatingPoint.mk_sort_64 ctx
