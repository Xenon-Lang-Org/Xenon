(module
  (func (export "cops") (result i32)
    i32.const 5
    i32.const 6
    i32.mul    ;; 5 * 6 = 30
    i32.const 10
    i32.add    ;; 30 + 10 = 40
    i32.const 2
    i32.sub    ;; 40 - 2  = 38
  )
)
