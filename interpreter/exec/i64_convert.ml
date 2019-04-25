(* WebAssembly-compatible type conversions to i64 implementation *)

let extend_i32_s x = I64.of_bits (Int64.of_int32 (I32.to_bits x))

let extend_i32_u x = I64.of_bits (Int64.logand (Int64.of_int32 (I32.to_bits x)) 0x00000000ffffffffL)

let trunc_f32_s x =
  if F32.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F32.to_float x in
    if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
      raise Numeric_error.IntegerOverflow
    else
      I64.of_bits (Int64.of_float xf)

let trunc_f32_u x =
  if F32.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F32.to_float x in
    if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
      raise Numeric_error.IntegerOverflow
    else if xf >= -.Int64.(to_float min_int) then
      I64.of_bits (Int64.(logxor (of_float (xf -. 9223372036854775808.0)) min_int))
    else
      I64.of_bits (Int64.of_float xf)

let trunc_f64_s x =
  if F64.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F64.to_float x in
    if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
      raise Numeric_error.IntegerOverflow
    else
      I64.of_bits (Int64.of_float xf)

let trunc_f64_u x =
  if F64.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F64.to_float x in
    if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
      raise Numeric_error.IntegerOverflow
    else if xf >= -.Int64.(to_float min_int) then
      I64.of_bits (Int64.(logxor (of_float (xf -. 9223372036854775808.0)) min_int))
    else
      I64.of_bits (Int64.of_float xf)

let reinterpret_f64 x = I64.of_bits (F64.to_bits x)
