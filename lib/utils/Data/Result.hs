module Data.Result
  ( Result (..),
    mapOk,
    mapErr,
    andThen,
    unwrap,
    unwrapOr,
    isOk,
    isErr,
  )
where

{- |
The 'Result' type represents a value that can be either an error or a success.
type @'Result' e a@ is either @'Ok' a@ or @'Err' e@.

The 'Result' type is used to handle errors in a functional way. It is similar to
the 'Either' type, but with the arguments flipped. This makes it easier to use
with the 'Functor', 'Applicative', and 'Monad' typeclasses.

The 'Result' type is used to represent the result of a computation that can fail.
It is used to handle errors in a functional way. It is similar to the 'Either'


==== __Examples__

>>> let divide x y = if y == 0 then Err "Division by zero" else Ok (x / y) :: Result String Double
>>> divide 10 2
Ok 5.0
>>> divide 10 0
Err "Division by zero"


The fmap from our 'Functor' instance will ignore the error value when applying the function.

>>> fmap (+ 1) (Ok 1)
Ok 2
>>> fmap (+ 1) (Err "error")
Err "error"

The <*> operator from our 'Applicative' instance will apply the function to the value if both are 'Ok'.

>>> (Ok (+ 1)) <*> (Ok 1)
Ok 2
>>> (Ok (+ 1)) <*> (Err "error")
Err "error"
>>> (Err "error") <*> (Ok 1)

The >>= operator from our 'Monad' instance will apply the function to the value if it is 'Ok'.

>>> (Ok 1) >>= (\x -> Ok (x + 1))
Ok 2
>>> (Err "error") >>= (\x -> Ok (x + 1))
Err "error"
>>> (Ok 1) >>= (\x -> Err "error")
Err "error"


The 'Eq' instance will compare the values inside the 'Ok' and 'Err' constructors.

>>> Ok 1 == Ok 1
True
>>> Ok 1 == Ok 2
False
>>> Err "error" == Err "error"
True
>>> Err "error" == Err "another error"
False




-}

data Result e a = Ok !a | Err !e
  deriving (Ord, Read, Show)

instance Functor (Result e) where
  fmap = mapOk

instance Applicative (Result e) where
  pure = Ok
  (Ok f) <*> (Ok x) = Ok (f x)
  (Err e) <*> _ = Err e
  _ <*> (Err e) = Err e

instance Monad (Result e) where
  return = pure
  (>>=) = andThen

instance (Eq o, Eq e) => Eq (Result o e) where
  (Ok x) == (Ok y) = x == y
  (Err x) == (Err y) = x == y
  _ == _ = False

-- | Maps a function over the 'Ok' value of a 'Result'.
-- If the 'Result' is an 'Err', the function is ignored.
mapOk :: (a -> b) -> Result e a -> Result e b
mapOk f (Ok x) = Ok (f x)
mapOk _ (Err e) = Err e

-- | Maps a function over the 'Err' value of a 'Result'.
-- If the 'Result' is an 'Ok', the function is ignored.
mapErr :: (e -> f) -> Result e a -> Result f a
mapErr _ (Ok x) = Ok x
mapErr f (Err e) = Err (f e)

-- | Chains a function that returns a 'Result' over the 'Ok' value of a 'Result'.
-- If the 'Result' is an 'Err', the function is ignored.
--
-- ==== __Examples__
--
-- >>> let divide x y = if y == 0 then Err "Division by zero" else Ok (x / y) :: Result String Double
-- >>> divide 5 2 `andThen` (\x -> divide x 2)
-- Ok 2.5
andThen :: Result e a -> (a -> Result e b) -> Result e b
andThen (Err e) _ = Err e
andThen (Ok x) f = f x

-- | Unwraps the value of a 'Result'. UNSAFE!
-- If the 'Result' is an 'Err', an error is thrown.
-- This function is unsafe and should be avoided. Only use it when you are sure that the 'Result' is an 'Ok'.
unwrap :: Result e a -> a
unwrap (Ok x) = x
unwrap (Err _) = error "Called unwrap on an Err value"

-- | Unwraps the value of a 'Result' or returns a default value.
-- If the 'Result' is an 'Err', the default value is returned.
-- This function is safe and should be used when you are not sure if the 'Result' is an 'Ok'.
unwrapOr :: a -> Result e a -> a
unwrapOr _ (Ok x) = x
unwrapOr defaultValue (Err _) = defaultValue

-- | Returns 'True' if the 'Result' is an 'Ok'.
isOk :: Result e a -> Bool
isOk (Ok _) = True
isOk (Err _) = False

-- Returns 'False' if the 'Result' is an 'Err'.
isErr :: Result e a -> Bool
isErr = not . isOk
