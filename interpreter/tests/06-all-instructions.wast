(module
 ;; Numeric Instructions
 (func (export "i32.const") (result i32) (i32.const 5))
 (func (export "i64.const") (result i64) (i64.const 5))

 (func (export "f32.const") (result f32) (f32.const 5.0))
 (func (export "f64.const") (result f64) (f64.const 5.0))

 ;; (func (export "i32.clz") (param $x i32) (result i32) (i32.clz (local.get $x)))
 ;; (func (export "i32.ctz") (param $x i32) (result i32) (i32.ctz (local.get $x)))
 ;; (func (export "i32.popcnt") (param $x i32) (result i32) (i32.popcnt (local.get $x)))
 ;; (func (export "i64.clz") (param $x i64) (result i64) (i64.clz (local.get $x)))
 ;; (func (export "i64.ctz") (param $x i64) (result i64) (i64.ctz (local.get $x)))
 ;; (func (export "i64.popcnt") (param $x i64) (result i64) (i64.popcnt (local.get $x)))

 (func (export "f32.abs") (param $x f32) (result f32) (f32.abs (local.get $x)))
 (func (export "f32.neg") (param $x f32) (result f32) (f32.neg (local.get $x)))
 (func (export "f32.sqrt") (param $x f32) (result f32) (f32.sqrt (local.get $x)))
 ;; (func (export "f32.ceil") (param $x f32) (result f32) (f32.ceil (local.get $x)))
 ;; (func (export "f32.floor") (param $x f32) (result f32) (f32.floor (local.get $x)))
 ;; (func (export "f32.trunc") (param $x f32) (result f32) (f32.trunc (local.get $x)))
 (func (export "f32.nearest") (param $x f32) (result f32) (f32.nearest (local.get $x)))
 (func (export "f64.abs") (param $x f64) (result f64) (f64.abs (local.get $x)))
 (func (export "f64.neg") (param $x f64) (result f64) (f64.neg (local.get $x)))
 (func (export "f64.sqrt") (param $x f64) (result f64) (f64.sqrt (local.get $x)))
 ;; (func (export "f64.ceil") (param $x f64) (result f64) (f64.ceil (local.get $x)))
 ;; (func (export "f64.floor") (param $x f64) (result f64) (f64.floor (local.get $x)))
 ;; (func (export "f64.trunc") (param $x f64) (result f64) (f64.trunc (local.get $x)))
 (func (export "f64.nearest") (param $x f64) (result f64) (f64.nearest (local.get $x)))

 (func (export "i32.add") (param $x i32) (param $y i32) (result i32) (i32.add (local.get $x) (local.get $y)))
 (func (export "i32.sub") (param $x i32) (param $y i32) (result i32) (i32.sub (local.get $x) (local.get $y)))
 (func (export "i32.mul") (param $x i32) (param $y i32) (result i32) (i32.mul (local.get $x) (local.get $y)))
 (func (export "i32.div_u") (param $x i32) (param $y i32) (result i32) (i32.div_u (local.get $x) (local.get $y)))
 (func (export "i32.div_s") (param $x i32) (param $y i32) (result i32) (i32.div_s (local.get $x) (local.get $y)))
 (func (export "i32.rem_u") (param $x i32) (param $y i32) (result i32) (i32.rem_u (local.get $x) (local.get $y)))
 (func (export "i32.rem_s") (param $x i32) (param $y i32) (result i32) (i32.rem_s (local.get $x) (local.get $y)))
 (func (export "i32.and") (param $x i32) (param $y i32) (result i32) (i32.and (local.get $x) (local.get $y)))
 (func (export "i32.or") (param $x i32) (param $y i32) (result i32) (i32.or (local.get $x) (local.get $y)))
 (func (export "i32.xor") (param $x i32) (param $y i32) (result i32) (i32.xor (local.get $x) (local.get $y)))
 (func (export "i32.shl") (param $x i32) (param $y i32) (result i32) (i32.shl (local.get $x) (local.get $y)))
 (func (export "i32.shr_u") (param $x i32) (param $y i32) (result i32) (i32.shr_u (local.get $x) (local.get $y)))
 (func (export "i32.shr_s") (param $x i32) (param $y i32) (result i32) (i32.shr_s (local.get $x) (local.get $y)))
 (func (export "i32.rotl") (param $x i32) (param $y i32) (result i32) (i32.rotl (local.get $x) (local.get $y)))
 (func (export "i32.rotr") (param $x i32) (param $y i32) (result i32) (i32.rotr (local.get $x) (local.get $y)))
 (func (export "i64.add") (param $x i64) (param $y i64) (result i64) (i64.add (local.get $x) (local.get $y)))
 (func (export "i64.sub") (param $x i64) (param $y i64) (result i64) (i64.sub (local.get $x) (local.get $y)))
 (func (export "i64.mul") (param $x i64) (param $y i64) (result i64) (i64.mul (local.get $x) (local.get $y)))
 (func (export "i64.div_u") (param $x i64) (param $y i64) (result i64) (i64.div_u (local.get $x) (local.get $y)))
 (func (export "i64.div_s") (param $x i64) (param $y i64) (result i64) (i64.div_s (local.get $x) (local.get $y)))
 (func (export "i64.rem_u") (param $x i64) (param $y i64) (result i64) (i64.rem_u (local.get $x) (local.get $y)))
 (func (export "i64.rem_s") (param $x i64) (param $y i64) (result i64) (i64.rem_s (local.get $x) (local.get $y)))
 (func (export "i64.and") (param $x i64) (param $y i64) (result i64) (i64.and (local.get $x) (local.get $y)))
 (func (export "i64.or") (param $x i64) (param $y i64) (result i64) (i64.or (local.get $x) (local.get $y)))
 (func (export "i64.xor") (param $x i64) (param $y i64) (result i64) (i64.xor (local.get $x) (local.get $y)))
 (func (export "i64.shl") (param $x i64) (param $y i64) (result i64) (i64.shl (local.get $x) (local.get $y)))
 (func (export "i64.shr_u") (param $x i64) (param $y i64) (result i64) (i64.shr_u (local.get $x) (local.get $y)))
 (func (export "i64.shr_s") (param $x i64) (param $y i64) (result i64) (i64.shr_s (local.get $x) (local.get $y)))
 (func (export "i64.rotl") (param $x i64) (param $y i64) (result i64) (i64.rotl (local.get $x) (local.get $y)))
 (func (export "i64.rotr") (param $x i64) (param $y i64) (result i64) (i64.rotr (local.get $x) (local.get $y)))

 (func (export "f32.add") (param $x f32) (param $y f32) (result f32) (f32.add (local.get $x) (local.get $y)))
 (func (export "f32.sub") (param $x f32) (param $y f32) (result f32) (f32.sub (local.get $x) (local.get $y)))
 (func (export "f32.mul") (param $x f32) (param $y f32) (result f32) (f32.mul (local.get $x) (local.get $y)))
 (func (export "f32.div") (param $x f32) (param $y f32) (result f32) (f32.div (local.get $x) (local.get $y)))
 (func (export "f32.min") (param $x f32) (param $y f32) (result f32) (f32.min (local.get $x) (local.get $y)))
 (func (export "f32.max") (param $x f32) (param $y f32) (result f32) (f32.max (local.get $x) (local.get $y)))
 ;; (func (export "f32.copysign") (param $x f32) (param $y f32) (result f32) (f32.copysign (local.get $x) (local.get $y)))
 (func (export "f64.add") (param $x f64) (param $y f64) (result f64) (f64.add (local.get $x) (local.get $y)))
 (func (export "f64.sub") (param $x f64) (param $y f64) (result f64) (f64.sub (local.get $x) (local.get $y)))
 (func (export "f64.mul") (param $x f64) (param $y f64) (result f64) (f64.mul (local.get $x) (local.get $y)))
 (func (export "f64.div") (param $x f64) (param $y f64) (result f64) (f64.div (local.get $x) (local.get $y)))
 (func (export "f64.min") (param $x f64) (param $y f64) (result f64) (f64.min (local.get $x) (local.get $y)))
 (func (export "f64.max") (param $x f64) (param $y f64) (result f64) (f64.max (local.get $x) (local.get $y)))
 ;; (func (export "f64.copysign") (param $x f64) (param $y f64) (result f64) (f64.copysign (local.get $x) (local.get $y)))

 (func (export "i32.eqz") (param $x i32) (result i32) (i32.eqz (local.get $x)))
 (func (export "i64.eqz") (param $x i64) (result i32) (i64.eqz (local.get $x)))

 (func (export "i32.eq") (param $x i32) (param $y i32) (result i32) (i32.eq (local.get $x) (local.get $y)))
 (func (export "i32.ne") (param $x i32) (param $y i32) (result i32) (i32.ne (local.get $x) (local.get $y)))
 (func (export "i32.lt_u") (param $x i32) (param $y i32) (result i32) (i32.lt_u (local.get $x) (local.get $y)))
 (func (export "i32.lt_s") (param $x i32) (param $y i32) (result i32) (i32.lt_s (local.get $x) (local.get $y)))
 (func (export "i32.gt_u") (param $x i32) (param $y i32) (result i32) (i32.gt_u (local.get $x) (local.get $y)))
 (func (export "i32.gt_s") (param $x i32) (param $y i32) (result i32) (i32.gt_s (local.get $x) (local.get $y)))
 (func (export "i32.le_u") (param $x i32) (param $y i32) (result i32) (i32.le_u (local.get $x) (local.get $y)))
 (func (export "i32.le_s") (param $x i32) (param $y i32) (result i32) (i32.le_s (local.get $x) (local.get $y)))
 (func (export "i32.ge_u") (param $x i32) (param $y i32) (result i32) (i32.ge_u (local.get $x) (local.get $y)))
 (func (export "i32.ge_s") (param $x i32) (param $y i32) (result i32) (i32.ge_s (local.get $x) (local.get $y)))
 (func (export "i64.eq") (param $x i64) (param $y i64) (result i32) (i64.eq (local.get $x) (local.get $y)))
 (func (export "i64.ne") (param $x i64) (param $y i64) (result i32) (i64.ne (local.get $x) (local.get $y)))
 (func (export "i64.lt_u") (param $x i64) (param $y i64) (result i32) (i64.lt_u (local.get $x) (local.get $y)))
 (func (export "i64.lt_s") (param $x i64) (param $y i64) (result i32) (i64.lt_s (local.get $x) (local.get $y)))
 (func (export "i64.gt_u") (param $x i64) (param $y i64) (result i32) (i64.gt_u (local.get $x) (local.get $y)))
 (func (export "i64.gt_s") (param $x i64) (param $y i64) (result i32) (i64.gt_s (local.get $x) (local.get $y)))
 (func (export "i64.le_u") (param $x i64) (param $y i64) (result i32) (i64.le_u (local.get $x) (local.get $y)))
 (func (export "i64.le_s") (param $x i64) (param $y i64) (result i32) (i64.le_s (local.get $x) (local.get $y)))
 (func (export "i64.ge_u") (param $x i64) (param $y i64) (result i32) (i64.ge_u (local.get $x) (local.get $y)))
 (func (export "i64.ge_s") (param $x i64) (param $y i64) (result i32) (i64.ge_s (local.get $x) (local.get $y)))

 (func (export "f32.eq") (param $x f32) (param $y f32) (result i32) (f32.eq (local.get $x) (local.get $y)))
 (func (export "f32.ne") (param $x f32) (param $y f32) (result i32) (f32.ne (local.get $x) (local.get $y)))
 (func (export "f32.lt") (param $x f32) (param $y f32) (result i32) (f32.lt (local.get $x) (local.get $y)))
 (func (export "f32.gt") (param $x f32) (param $y f32) (result i32) (f32.gt (local.get $x) (local.get $y)))
 (func (export "f32.le") (param $x f32) (param $y f32) (result i32) (f32.le (local.get $x) (local.get $y)))
 (func (export "f32.ge") (param $x f32) (param $y f32) (result i32) (f32.ge (local.get $x) (local.get $y)))
 (func (export "f64.eq") (param $x f64) (param $y f64) (result i32) (f64.eq (local.get $x) (local.get $y)))
 (func (export "f64.ne") (param $x f64) (param $y f64) (result i32) (f64.ne (local.get $x) (local.get $y)))
 (func (export "f64.lt") (param $x f64) (param $y f64) (result i32) (f64.lt (local.get $x) (local.get $y)))
 (func (export "f64.gt") (param $x f64) (param $y f64) (result i32) (f64.gt (local.get $x) (local.get $y)))
 (func (export "f64.le") (param $x f64) (param $y f64) (result i32) (f64.le (local.get $x) (local.get $y)))
 (func (export "f64.ge") (param $x f64) (param $y f64) (result i32) (f64.ge (local.get $x) (local.get $y)))

 ;; (func (export "i32.wrap_i64") (param $x i64) (result i32) (i32.wrap_i64 (local.get $x)))

 (func (export "i64.extend_i32_u") (param $x i32) (result i64) (i64.extend_i32_u (local.get $x)))
 (func (export "i64.extend_i32_s") (param $x i32) (result i64) (i64.extend_i32_s (local.get $x)))

 (func (export "i32.trunc_f32_u") (param $x f32) (result i32) (i32.trunc_f32_u (local.get $x)))
 (func (export "i32.trunc_f32_s") (param $x f32) (result i32) (i32.trunc_f32_s (local.get $x)))
 (func (export "i32.trunc_f64_u") (param $x f64) (result i32) (i32.trunc_f64_u (local.get $x)))
 (func (export "i32.trunc_f64_s") (param $x f64) (result i32) (i32.trunc_f64_s (local.get $x)))
 (func (export "i64.trunc_f32_u") (param $x f32) (result i64) (i64.trunc_f32_u (local.get $x)))
 (func (export "i64.trunc_f32_s") (param $x f32) (result i64) (i64.trunc_f32_s (local.get $x)))
 (func (export "i64.trunc_f64_u") (param $x f64) (result i64) (i64.trunc_f64_u (local.get $x)))
 (func (export "i64.trunc_f64_s") (param $x f64) (result i64) (i64.trunc_f64_s (local.get $x)))

 (func (export "f32.demote_f64") (param $x f64) (result f32) (f32.demote_f64 (local.get $x)))

 (func (export "f64.promote_f32") (param $x f32) (result f64) (f64.promote_f32 (local.get $x)))

 ;; (func (export "f32.convert_i32_u") (param $x i32) (result f32) (f32.convert_i32_u (local.get $x)))
 ;; (func (export "f32.convert_i32_s") (param $x i32) (result f32) (f32.convert_i32_s (local.get $x)))
 ;; (func (export "f32.convert_i64_u") (param $x i64) (result f32) (f32.convert_i64_u (local.get $x)))
 ;; (func (export "f32.convert_i64_s") (param $x i64) (result f32) (f32.convert_i64_s (local.get $x)))
 ;; (func (export "f64.convert_i32_u") (param $x i32) (result f64) (f64.convert_i32_u (local.get $x)))
 ;; (func (export "f64.convert_i32_s") (param $x i32) (result f64) (f64.convert_i32_s (local.get $x)))
 ;; (func (export "f64.convert_i64_u") (param $x i64) (result f64) (f64.convert_i64_u (local.get $x)))
 ;; (func (export "f64.convert_i64_s") (param $x i64) (result f64) (f64.convert_i64_s (local.get $x)))

 (func (export "i32.reinterpret_f32") (param $x f32) (result i32) (i32.reinterpret_f32 (local.get $x)))

 (func (export "f32.reinterpret_i32") (param $x i32) (result f32) (f32.reinterpret_i32 (local.get $x)))

 ;; Parametric Instructions
 (func (export "drop") (param $x i32) (drop (local.get $x)))
 ;; (func (export "select") (param $x i32) (param $y i32) (param $z i32) (result i32) (select (local.get $x) (local.get $y) (local.get $z)))

 ;; Variable Instructions (all work)
 ;; func local.get
 ;; func local.set
 ;; func local.tee
 ;; func global.get
 ;; func global.set

 ;; Memory Instructions (I won't support these to start)
 ;; i32.load
 ;; i64.load

 ;; f32.load
 ;; f64.load

 ;; i32.store
 ;; i64.store

 ;; f32.store
 ;; f64.store


 ;; Control Instructions
 (func (export "nop") (nop))
 (func (export "unreachable") (unreachable))
 (func (export "block") (result i32) (block (result i32) (br 0 (i32.const 1)) (i32.const 2)))
 (func (export "loop") (loop (br 0)))
 (func (export "if") (param $x i32) (if (local.get $x) (then (nop)) (else (nop))))
 (func (export "br") (block (block (block (br 2)))))
 (func (export "br_if") (param $x i32) (block (br_if 0 (local.get $x))))
 ;; func br_table
 (func (export "return") (block (block (block (return)))))
 (func $f (export "call") (call $g))
 (func $g (export "call2") (call $f))
 ;; func call_indirect
)
