(module
  (func $fibo (param $n i32) (result i32)
    local.get $n
    i32.const 2
    i32.lt_s
    if (result i32)
      local.get $n
    else
      local.get $n
      i32.const 1
      i32.sub
      call $fibo
      local.get $n
      i32.const 2
      i32.sub
      call $fibo
      i32.add
    end
  )
  (export "fibo" (func $fibo))
)
