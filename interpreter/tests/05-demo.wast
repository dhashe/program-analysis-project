(module
 (func (export "add") (param $x i32) (param $y i32) (result i32)
       (i32.add (local.get $x) (local.get $y)))

 (func (export "div") (param $x i32) (param $y i32) (result i32)
       (i32.div_s (local.get $x) (local.get $y)))

 (func (export "safediv") (param $x i32) (param $y i32) (result i32)
       (if (result i32) (local.get $y)
           (then (i32.div_u (local.get $x) (local.get $y)))
           (else (i32.const 1))))

 (func (export "reach") (param $x i32)
       (unreachable))

 (func (export "infinite_loop") (loop (br 0)))

 ;; Memory not supported
 ;; (func (export "store") (param $x i32) (result i32)
 ;;       (i32.store16 (i32.const 8) (local.get $x)) (i32.load16_s (i32.const 8)))
)

(symbolic_invoke "add" (i32.sym %x) (i32.sym %y)) ;; No errors, so no output
(symbolic_invoke "div" (i32.sym %x) (i32.sym %y)) ;; Divide by zero
(symbolic_invoke "safediv" (i32.sym %x) (i32.sym %y)) ;; No divide by zero
(symbolic_invoke "reach" (i32.sym %x)) ;; Unreachable
(symbolic_invoke "infinite_loop") ;; No errors, algorithm detects loop and stops
