type 'xXX concreteness = Concrete of 'xXX | Symbolic of Z3.Expr.expr

let was_concrete x = match x with
    Concrete y -> y
  | Symbolic _ -> failwith "Was not concrete"

let ctx = Z3.mk_context []

let solver = Z3.Solver.mk_solver ctx None

let try_constraints constraints =
  let () = Z3.Solver.push solver in
  let () = Z3.Solver.add solver constraints in
  let _ = Z3.Solver.check solver [] in
  let model = Z3.Solver.get_model solver in
  model

let fp_round = Z3.FloatingPoint.RoundingMode.mk_rne ctx
let fp_sort32 = Z3.FloatingPoint.mk_sort_32 ctx
let fp_sort64 = Z3.FloatingPoint.mk_sort_64 ctx
