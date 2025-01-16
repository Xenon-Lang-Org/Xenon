(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (param i32 i32) (result i32)))
  (type (;3;) (func (result i32)))
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
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32 i32)
    i32.const 2
    local.set 1
    local.get 0
    i32.const 0
    i32.eq
    local.get 0
    i32.const 1
    i32.eq
    i32.or
    if  ;; label = @1
      i32.const 0
      return
    end
    block  ;; label = @1
      loop  ;; label = @2
        local.get 1
        local.get 0
        i32.lt_s
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        local.get 1
        i32.rem_s
        i32.const 0
        i32.eq
        if  ;; label = @3
          i32.const 0
          return
        end
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        br 0 (;@2;)
      end
    end
    i32.const 1
    return)
  (func (;2;) (type 2) (param i32 i32) (result i32)
    (local i32 i32)
    local.get 0
    local.get 1
    i32.add
    return)
  (func (;3;) (type 3) (result i32)
    (local i32 i32)
    i32.const 2
    call 1
    i32.const 3
    call 1
    call 2
    local.set 0
    i32.const 0
    local.set 1
    local.get 0
    global.get 0
    call 0
    local.set 1
    local.get 1
    return)
  (global (;0;) (mut i32) (i32.const 8))
  (export "pow" (func 0))
  (export "is_prime" (func 1))
  (export "add" (func 2))
  (export "main" (func 3)))
