(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param i32 i32) (result i32)))
  (type (;2;) (func (param i32 i32) (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.get 1
    i32.add
    return)
  (func (;1;) (type 1) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.get 1
    i32.sub
    return)
  (func (;2;) (type 2) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.get 1
    i32.mul
    return)
  (func (;3;) (type 3) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.get 1
    i32.div_s
    return)
  (func (;4;) (type 4) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.get 1
    i32.rem_s
    return)
  (export "add" (func 0))
  (export "sub" (func 1))
  (export "mul" (func 2))
  (export "div" (func 3))
  (export "mod" (func 4)))
