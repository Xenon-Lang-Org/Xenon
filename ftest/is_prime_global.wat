(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32)
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
        global.get 0
        local.get 0
        i32.lt_s
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        global.get 0
        i32.rem_s
        i32.const 0
        i32.eq
        if  ;; label = @3
          i32.const 0
          return
        end
        global.get 0
        i32.const 1
        i32.add
        global.set 0
        br 0 (;@2;)
      end
    end
    i32.const 1
    return)
  (global (;0;) (mut i32) (i32.const 2))
  (export "is_prime" (func 0)))
