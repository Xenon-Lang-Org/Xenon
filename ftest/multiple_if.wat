(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (type (;3;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.const 10
    i32.gt_s
    if  ;; label = @1
      i32.const 1
      return
    else
      local.get 0
      i32.const 5
      i32.gt_s
      if  ;; label = @2
        i32.const 2
        return
      else
        i32.const 0
        return
      end
    end
    i32.const 3
    return)
  (func (;1;) (type 1) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.const 1
    i32.eq
    if  ;; label = @1
      i32.const 1
      return
    else
      local.get 0
      i32.const 2
      i32.eq
      if  ;; label = @2
        i32.const 2
        return
      else
        local.get 0
        i32.const 3
        i32.eq
        if  ;; label = @3
          i32.const 3
          return
        else
          local.get 0
          i32.const 4
          i32.eq
          if  ;; label = @4
            i32.const 4
            return
          else
            i32.const 0
            return
          end
        end
      end
    end
    i32.const 0
    return)
  (func (;2;) (type 2) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.const 10
    i32.ge_s
    if  ;; label = @1
      i32.const 1
      return
    end
    i32.const 0
    return)
  (func (;3;) (type 3) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.const 10
    i32.le_s
    if  ;; label = @1
      i32.const 1
      return
    end
    i32.const 0
    return)
  (export "doelif" (func 0))
  (export "forest" (func 1))
  (export "supegal" (func 2))
  (export "infegal" (func 3)))
