{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Control.Convert
  ( Into (..),
    From (..),
    TryFrom (..),
    TryInto (..),
  )
where
import Utils.Data.Result (Result(..))


-- | The 'Into' typeclass is used to convert a value of type @a@ into a value of type @b@.
--
-- ==== __Examples__
--
-- >>> into (Ok 1) :: Maybe Int
-- Just 1
-- >>> into (Err "error") :: Maybe Int
-- Nothing
-- >>> into (Ok 1) :: Either String Int
-- Right 1
class Into a b where
  into :: a -> b

class (Into a b) => From a b where
  from :: a -> b
  from = into

class TryFrom a b where
  tryFrom :: a -> Maybe b

class (TryFrom a b) => TryInto a b where
  tryInto :: a -> Maybe b
  tryInto = tryFrom

-- |
-- Instances of 'Into' for common types.
instance Into Int Double where
  into = fromIntegral

instance (Into Double Int) => From Double Int where
  from = round

instance TryFrom String Int where
  tryFrom s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

instance Into (Result e a) (Maybe a) where
  into (Ok x) = Just x
  into (Err _) = Nothing

instance Into (Result e a) (Either e a) where
  into (Ok x) = Right x
  into (Err e) = Left e

instance (Into a b) => Into (Maybe a) (Result e b) where
  into (Just x) = Ok (into x)
  into Nothing = Err undefined

instance (Into a b) => Into (Result e a) (Result e b) where
  into (Ok x) = Ok (into x)
  into (Err e) = Err e

instance (TryFrom a b) => TryFrom (Result e a) b where
  tryFrom (Ok x) = tryFrom x
  tryFrom (Err _) = Nothing

instance (TryInto a b) => TryInto (Result e a) b where
  tryInto (Ok x) = tryInto x
  tryInto (Err _) = Nothing

instance (TryFrom a b) => TryFrom (Maybe a) b where
  tryFrom (Just x) = tryFrom x
  tryFrom Nothing = Nothing

instance (TryInto a b) => TryInto (Maybe a) b where
  tryInto (Just x) = tryInto x
  tryInto Nothing = Nothing

instance (TryFrom a b) => TryFrom [a] [b] where
  tryFrom = traverse tryFrom

instance (TryInto a b) => TryInto [a] [b] where
  tryInto = traverse tryInto
