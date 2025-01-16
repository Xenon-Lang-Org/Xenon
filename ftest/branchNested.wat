(module
  (func (export "bnest") (param $x i32) (result i32)
    (local $result i32)
    block $outer
      local.get $x
      i32.const 0
      i32.eq
      br_if $outer
      local.get $x
      i32.const 1
      i32.add
      local.set $result
      br $outer
    end
    local.get $result
  )
)
