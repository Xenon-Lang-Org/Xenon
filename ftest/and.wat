(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    i32.const 10
    i32.gt_s
    local.get 1
    i32.const 10
    i32.gt_s
    i32.and
    if  ;; label = @1
      i32.const 1
      return
    else
      i32.const 0
      return
    end
    i32.const 2
    return)
  (export "ifand" (func 0)))
