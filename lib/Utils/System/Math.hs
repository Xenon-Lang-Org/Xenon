module Utils.System.Math
  ( safeDiv, )
where

import Utils.Data.Result (Result(..))


safeDiv :: (Integral a) => a -> a -> Result String a
safeDiv _ 0 = Err "Division by zero"
safeDiv x y = Ok (x `div` y)
