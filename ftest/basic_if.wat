(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.const 10
    i32.gt_s
    if  ;; label = @1
      i32.const 1
      return
    else
      i32.const 0
      return
    end
    i32.const 2
    return)
  (export "doif" (func 0)))
