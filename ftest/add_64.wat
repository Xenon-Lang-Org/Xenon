(module
  (type (;0;) (func (param i64 i64) (result i64)))
  (func (;0;) (type 0) (param i64 i64) (result i64)
    (local i64 i64)
    local.get 0
    local.get 1
    i64.add
    return)
  (export "add" (func 0)))
