(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32)
    i32.const 0
    local.set 2
    block  ;; label = @1
      loop  ;; label = @2
        local.get 1
        i32.const 0
        i32.ne
        i32.eqz
        br_if 1 (;@1;)
        local.get 1
        local.set 2
        local.get 0
        local.get 1
        i32.rem_s
        local.set 1
        local.get 2
        local.set 0
        br 0 (;@2;)
      end
    end
    local.get 0
    return)
  (export "gcd" (func 0)))
