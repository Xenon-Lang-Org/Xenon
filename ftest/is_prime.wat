(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
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
  (export "is_prime" (func 0)))
