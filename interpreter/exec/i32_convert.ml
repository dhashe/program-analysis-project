(* WebAssembly-compatible type conversions to i32 implementation *)

(* NOTE I suppose that these have to be separate from the structures because
   they involve types from multiple structures.
*)

module C = Concreteness

let wrap_i64' x = I32.of_bits (Int64.to_int32 (I64.to_bits x))
let wrap_i64'' x = Z3.BitVector.mk_extract C.ctx 0 31 x
let wrap_i64 x = match x with
    C.Concrete _  -> wrap_i64'  x
  | C.Symbolic x' -> C.Symbolic (wrap_i64'' x')


let trunc_f32_s x = match x with
    C.Concrete _ -> (
      if C.was_concrete (F32.ne x x) then
        raise Numeric_error.InvalidConversionToInteger
      else
        let xf = F32.to_float x in
        if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) then
          raise Numeric_error.IntegerOverflow
        else
          I32.of_bits (Int32.of_float xf)
    )
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_sbv C.ctx C.fp_round x' 32)

let trunc_f32_u x = match x with
    C.Concrete _ -> (
      if C.was_concrete (F32.ne x x) then
        raise Numeric_error.InvalidConversionToInteger
      else
        let xf = F32.to_float x in
        if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0 then
          raise Numeric_error.IntegerOverflow
        else
          I32.of_bits (Int64.(to_int32 (of_float xf)))
    )
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_ubv C.ctx C.fp_round x' 32)

let trunc_f64_s x = match x with
    C.Concrete _ -> (
      if C.was_concrete (F64.ne x x) then
        raise Numeric_error.InvalidConversionToInteger
      else
        let xf = F64.to_float x in
        if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) then
          raise Numeric_error.IntegerOverflow
        else
          I32.of_bits (Int32.of_float xf)
    )
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_sbv C.ctx C.fp_round x' 64)


let trunc_f64_u x = match x with
    C.Concrete _ -> (
      if C.was_concrete (F64.ne x x) then
        raise Numeric_error.InvalidConversionToInteger
      else
        let xf = F64.to_float x in
        if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0 then
          raise Numeric_error.IntegerOverflow
        else
          I32.of_bits (Int64.(to_int32 (of_float xf)))
    )
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_ubv C.ctx C.fp_round x' 64)

let reinterpret_f32 x = match x with
    C.Concrete _ -> I32.of_bits (F32.to_bits x)
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_ieee_bv C.ctx x')
