(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    local.get 0
    local.get 1
    i32.and
    local.set 2
    local.get 0
    local.get 1
    i32.or
    local.set 3
    local.get 0
    local.get 1
    i32.xor
    local.set 4
    local.get 2
    local.get 3
    i32.add
    local.get 4
    i32.add
    return)
  (export "bitwise" (func 0)))
