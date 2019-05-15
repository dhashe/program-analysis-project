(* WebAssembly-compatible type conversions to i64 implementation *)

module C = Concreteness

let extend_i32_s x = match x with
    C.Concrete _ -> I64.of_bits (Int64.of_int32 (I32.to_bits x))
  | C.Symbolic x' -> C.Symbolic (Z3.BitVector.mk_sign_ext C.ctx 32 x')

let extend_i32_u x = match x with
    C.Concrete _ -> I64.of_bits (Int64.logand (Int64.of_int32 (I32.to_bits x)) 0x00000000ffffffffL)
  | C.Symbolic x' -> C.Symbolic (Z3.BitVector.mk_zero_ext C.ctx 32 x')

let trunc_f32_s x = match x with
  C.Concrete _ -> (
    if C.was_concrete (F32.ne x x) then
      raise Numeric_error.InvalidConversionToInteger
    else
      let xf = F32.to_float x in
      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
        raise Numeric_error.IntegerOverflow
      else
        I64.of_bits (Int64.of_float xf)
)
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_sbv C.ctx C.fp_round x' 64)

let trunc_f32_u x =  match x with
    C.Concrete _ -> (
      if C.was_concrete (F32.ne x x) then
        raise Numeric_error.InvalidConversionToInteger
      else
        let xf = F32.to_float x in
        if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
          raise Numeric_error.IntegerOverflow
        else if xf >= -.Int64.(to_float min_int) then
          I64.of_bits (Int64.(logxor (of_float (xf -. 9223372036854775808.0)) min_int))
        else
          I64.of_bits (Int64.of_float xf)
    )
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_ubv C.ctx C.fp_round x' 64)

let trunc_f64_s x = match x with
    C.Concrete _ -> (
      if C.was_concrete (F64.ne x x) then
        raise Numeric_error.InvalidConversionToInteger
      else
        let xf = F64.to_float x in
        if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
          raise Numeric_error.IntegerOverflow
        else
          I64.of_bits (Int64.of_float xf)
    )
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_sbv C.ctx C.fp_round x' 64)

let trunc_f64_u x = match x with
  C.Concrete _ -> (
    if C.was_concrete (F64.ne x x) then
      raise Numeric_error.InvalidConversionToInteger
    else
      let xf = F64.to_float x in
      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
        raise Numeric_error.IntegerOverflow
      else if xf >= -.Int64.(to_float min_int) then
        I64.of_bits (Int64.(logxor (of_float (xf -. 9223372036854775808.0)) min_int))
      else
        I64.of_bits (Int64.of_float xf)
  )
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_ubv C.ctx C.fp_round x' 64)

let reinterpret_f64 x = match x with
    C.Concrete _ -> I64.of_bits (F64.to_bits x)
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_ieee_bv C.ctx x')
