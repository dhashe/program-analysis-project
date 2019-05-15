(* WebAssembly-compatible type conversions to f64 implementation *)

module C = Concreteness

let promote_f32' x =
  let xf = F32.to_float x in
  if xf = xf then F64.of_float xf else
  let nan32bits = I64_convert.extend_i32_u (I32.of_bits (F32.to_bits x)) in
  let sign_field = Int64.(shift_left (shift_right_logical (I64.to_bits nan32bits) 31) 63) in
  let significand_field = Int64.(shift_right_logical (shift_left (I64.to_bits nan32bits) 41) 12) in
  let fields = Int64.logor sign_field significand_field in
  let nan64bits = Int64.logor 0x7ff8000000000000L fields in
  F64.of_bits nan64bits
let promote_f32'' x = Z3.FloatingPoint.mk_to_fp_float C.ctx C.fp_round x C.fp_sort64
let promote_f32 x = match x with
    C.Concrete _ -> promote_f32' x
  | C.Symbolic x' -> C.Symbolic (promote_f32'' x')

let convert_i32_s x = match x with
    C.Concrete _ -> F64.of_float (Int32.to_float (I32.to_bits x))
  | C.Symbolic x' ->
    let bv = Z3.BitVector.mk_bv2int C.ctx x' true (* is_signed *) in
    C.Symbolic (Z3.FloatingPoint.mk_to_fp_signed C.ctx C.fp_round bv C.fp_sort64)

(*
 * Unlike the other convert_u functions, the high half of the i32 range is
 * within the range where f32 can represent odd numbers, so we can't do the
 * shift. Instead, we can use int64 signed arithmetic.
 *)
let convert_i32_u x = match x with
    C.Concrete _ -> F64.of_float Int64.(to_float (logand (of_int32 (I32.to_bits x)) 0x00000000ffffffffL))
  | C.Symbolic x' ->
    let bv = Z3.BitVector.mk_bv2int C.ctx x' false (* is_signed *) in
    C.Symbolic (Z3.FloatingPoint.mk_to_fp_signed C.ctx C.fp_round bv C.fp_sort64)

let convert_i64_s x = match x with
    C.Concrete _ -> F64.of_float (Int64.to_float (I64.to_bits x))
  | C.Symbolic x' ->
    let bv = Z3.BitVector.mk_bv2int C.ctx x' true (* is_signed *) in
    C.Symbolic (Z3.FloatingPoint.mk_to_fp_signed C.ctx C.fp_round bv C.fp_sort64)

(*
 * Values in the low half of the int64 range can be converted with a signed
 * conversion. The high half is beyond the range where f64 can represent odd
 * numbers, so we can shift the value right, adjust the least significant
 * bit to round correctly, do a conversion, and then scale it back up.
 *)
let convert_i64_u x = match x with
    C.Concrete _ -> 
    let x' = I64.to_bits x in
    F64.of_float
      Int64.(if x' >= zero then to_float x' else
               to_float (logor (shift_right_logical x' 1) (logand x' 1L)) *. 2.0)
  | C.Symbolic x' ->
    let bv = Z3.BitVector.mk_bv2int C.ctx x' false (* is_signed *) in
    C.Symbolic (Z3.FloatingPoint.mk_to_fp_signed C.ctx C.fp_round bv C.fp_sort32)

let reinterpret_i64 x = match x with
    C.Concrete _ -> F64.of_bits (I64.to_bits x)
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_fp_bv C.ctx x' C.fp_sort64)
