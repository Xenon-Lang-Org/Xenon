(module
  (func (export "loop") (param $n i32) (result i32)
    (local $sum i32)
    block $exit
      loop $loop
        local.get $n
        i32.eqz
        br_if $exit
        local.get $sum
        local.get $n
        i32.add
        local.set $sum
        local.get $n
        i32.const 1
        i32.sub
        local.set $n
        br $loop
      end
    end
    local.get $sum
  )
)
