(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32 i32)
    i32.const 1
    local.set 2
    local.get 0
    local.set 3
    local.get 1
    i32.const 0
    i32.eq
    if  ;; label = @1
      i32.const 1
      return
    end
    local.get 1
    i32.const 1
    i32.eq
    if  ;; label = @1
      local.get 0
      return
    end
    block  ;; label = @1
      loop  ;; label = @2
        local.get 2
        local.get 1
        i32.lt_s
        i32.eqz
        br_if 1 (;@1;)
        local.get 3
        local.get 0
        i32.mul
        local.set 3
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        br 0 (;@2;)
      end
    end
    local.get 3
    return)
  (export "pow" (func 0)))
