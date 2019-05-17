(module
 (func (export "notes_example") (param $a i32) (param $b i32) (param $c i32)
       (local $x i32) (local $y i32) (local $z i32)
       (block
        (if (local.get $a)
            (then (local.set $x (i32.const -2)))
            (else (local.set $x (i32.const 0))))
        (if (i32.lt_s (local.get $b) (i32.const 5))
            (then (block
                   (if (i32.and (i32.eqz (local.get $a)) (local.get $c))
                      (then (local.set $y (i32.const 1)))
                      (else (nop)))
                   (local.set $z (i32.const 2))))
            (else (nop)))
        (if (i32.ne (i32.add (local.get $x) (i32.add (local.get $y) (local.get $z))) (i32.const 3))
            (then (nop))
            (else (unreachable)))
        )))

;; (invoke "notes_example" (i32.const 0) (i32.const 2) (i32.const 3))
(symbolic_invoke "notes_example" (i32.sym %a) (i32.sym %b) (i32.sym %c))
