(* WebAssembly-compatible type conversions to i32 implementation *)

(* NOTE I suppose that these have to be separate from the structures because
   they involve types from multiple structures.
*)

let wrap_i64 x = I32.of_bits (Int64.to_int32 (I64.to_bits x))

let trunc_f32_s x =
  if F32.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F32.to_float x in
    if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) then
      raise Numeric_error.IntegerOverflow
    else
      I32.of_bits (Int32.of_float xf)

let trunc_f32_u x =
  if F32.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F32.to_float x in
    if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0 then
      raise Numeric_error.IntegerOverflow
    else
      I32.of_bits (Int64.(to_int32 (of_float xf)))

let trunc_f64_s x =
  if F64.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F64.to_float x in
    if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) then
      raise Numeric_error.IntegerOverflow
    else
      I32.of_bits (Int32.of_float xf)

let trunc_f64_u x =
  if F64.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F64.to_float x in
    if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0 then
      raise Numeric_error.IntegerOverflow
    else
      I32.of_bits (Int64.(to_int32 (of_float xf)))

let reinterpret_f32 x = I32.of_bits (F32.to_bits x)
