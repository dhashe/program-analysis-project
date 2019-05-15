(* WebAssembly-compatible type conversions to f32 implementation *)

module C = Concreteness

let demote_f64' x =
  let xf = F64.to_float x in
  if xf = xf then F32.of_float xf else
  let nan64bits = F64.to_bits x in
  let sign_field = Int64.(shift_left (shift_right_logical nan64bits 63) 31) in
  let significand_field = Int64.(shift_right_logical (shift_left nan64bits 12) 41) in
  let fields = Int64.logor sign_field significand_field in
  let nan32bits = Int32.logor 0x7fc00000l (I32.to_bits (I32_convert.wrap_i64 (I64.of_bits fields))) in
  F32.of_bits nan32bits
let demote_f64'' x = Z3.FloatingPoint.mk_to_fp_float C.ctx C.fp_round x C.fp_sort32
let demote_f64 x = match x with
    C.Concrete _ -> demote_f64' x
  | C.Symbolic x' -> C.Symbolic (demote_f64'' x')

let convert_i32_s x = match x with
    C.Concrete _ -> F32.of_float (Int32.to_float (I32.to_bits x))
  | C.Symbolic x' ->
    let bv = Z3.BitVector.mk_bv2int C.ctx x' true (* is_signed *) in
    C.Symbolic (Z3.FloatingPoint.mk_to_fp_signed C.ctx C.fp_round bv C.fp_sort32)

(*
 * Similar to convert_i64_u below, the high half of the i32 range are beyond
 * the range where f32 can represent odd numbers, though we do need to adjust
 * the least significant bit to round correctly.
 *)
let convert_i32_u x = match x with
    C.Concrete _ ->
    let x' = I32.to_bits x in
    F32.of_float
      Int32.(if x' >= zero then to_float x' else
               to_float (logor (shift_right_logical x' 1) (logand x' 1l)) *. 2.0)
  | C.Symbolic x' ->
    let bv = Z3.BitVector.mk_bv2int C.ctx x' false (* is_signed *) in
    C.Symbolic (Z3.FloatingPoint.mk_to_fp_signed C.ctx C.fp_round bv C.fp_sort32)

let convert_i64_s x = match x with
    C.Concrete _ -> F32.of_float (Int64.to_float (I64.to_bits x))
  | C.Symbolic x' ->
    let bv = Z3.BitVector.mk_bv2int C.ctx x' true (* is_signed *) in
    C.Symbolic (Z3.FloatingPoint.mk_to_fp_signed C.ctx C.fp_round bv C.fp_sort32)

(*
 * Values in the low half of the int64 range can be converted with a signed
 * conversion. The high half is beyond the range where f32 can represent odd
 * numbers, so we can shift the value right, do a conversion, and then scale it
 * back up, without worrying about losing the least-significant digit.
 *)
let convert_i64_u x = match x with
    C.Concrete _ -> (
      let x' = I64.to_bits x in
      F32.of_float (if x' >= Int64.zero then
                      Int64.to_float x'
                    else
                      Int64.(to_float (shift_right_logical x' 1) *. 2.0))
    )
  | C.Symbolic x' ->
    let bv = Z3.BitVector.mk_bv2int C.ctx x' false (* is_signed *) in
    C.Symbolic (Z3.FloatingPoint.mk_to_fp_signed C.ctx C.fp_round bv C.fp_sort32)

let reinterpret_i32 x = match x with
    C.Concrete _ -> F32.of_bits (I32.to_bits x)
  | C.Symbolic x' -> C.Symbolic (Z3.FloatingPoint.mk_to_fp_bv C.ctx x' C.fp_sort32)
