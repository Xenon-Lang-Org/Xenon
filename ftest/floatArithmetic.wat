(module
  (func (export "farithm") (param $a f32) (param $b f32) (result f32)
    local.get $a
    local.get $b
    f32.add
  )
)
