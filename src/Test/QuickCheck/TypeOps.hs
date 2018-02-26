{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Test.QuickCheck.TypeOps
  ( type Each
  , type With
  ) where
  
import Data.Kind (Constraint)

-- | Map several constraints over several variables.
--
-- @
-- f :: Each [Show, Read] [a, b] => a -> b -> String
-- =
-- f :: (Show a, Show b, Read a, Read b) => a -> b -> String
-- @
--
-- To specify list with single constraint / variable, prefix
-- it with @\'@:
--
-- @
-- f :: Each '[Show] [a, b] => a -> b -> String
-- @ 
type family Each (c :: [k -> Constraint]) (as :: [k]) where
  Each c '[] = (() :: Constraint)
  Each c (h ': t) = (With c h, Each c t)

-- | Map several constraints over a single variable.
--
-- @
-- f :: With [Show, Read] a => a -> a
-- =
-- f :: (Show a, Read a) => a -> a
-- @ 
type family With (c :: [k -> Constraint]) (a :: k) where
    With '[] a = (() :: Constraint) 
    With (ch ': ct) a = (ch a, With ct a)
