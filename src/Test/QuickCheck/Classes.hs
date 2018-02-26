{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

{-|

This library provides lists of properties that should hold for common typeclasses.
All of these take a 'Proxy' argument that is used to nail down the type for which
the typeclass dictionaries should be tested. For example, at GHCi:

>>> lawsCheck (monoidLaws (Proxy :: Proxy Ordering))
Monoid: Associative +++ OK, passed 100 tests.
Monoid: Left Identity +++ OK, passed 100 tests.
Monoid: Right Identity +++ OK, passed 100 tests.

Assuming that the 'Arbitrary' instance for 'Ordering' is good, we now
have confidence that the 'Monoid' instance for 'Ordering' satisfies
the monoid laws. We can check multiple typeclasses with:

>>> foldMap (lawsCheck . ($ (Proxy :: Proxy Word))) [jsonLaws,showReadLaws]
ToJSON/FromJSON: Encoding Equals Value +++ OK, passed 100 tests.
ToJSON/FromJSON: Partial Isomorphism +++ OK, passed 100 tests.
Show/Read: Partial Isomorphism +++ OK, passed 100 tests.

-}
module Test.QuickCheck.Classes
  ( -- * Running
    lawsCheck
  , lawsCheckMany
    -- * Properties
    -- ** Ground Types
  , semigroupLaws
  , monoidLaws
  , commutativeMonoidLaws
  , eqLaws
  , ordLaws
  , showReadLaws
  , jsonLaws
  , isListLaws
  , primLaws
  , storableLaws
  , integralLaws
  , bitsLaws
#if MIN_VERSION_QuickCheck(2,10,0)
    -- ** Higher-Kinded Types
  , functorLaws
  , applicativeLaws
  , monadLaws
  , foldableLaws
#endif
    -- * Types
  , Laws(..)
  ) where

import Control.Applicative (liftA2)
import Control.Monad.ST
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Bits
import Data.Foldable (foldMap)
import Data.Primitive hiding (sizeOf,newArray,copyArray)
import Data.Primitive.Addr (Addr(..))
import Data.Primitive.PrimArray
import Data.Proxy
import Data.Semigroup (Semigroup)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Exts (IsList(fromList,toList,fromListN),Item)
import GHC.Ptr (Ptr(..))
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property(..))
import Test.QuickCheck.TypeOps
import Text.Read (readMaybe)
import qualified Data.Aeson as AE
import qualified Data.Primitive as P
import qualified Data.Semigroup as SG
import qualified GHC.OldList as L
import qualified Data.Set as S

#if MIN_VERSION_QuickCheck(2,10,0)
import Control.Exception (ErrorCall,try,evaluate)
import Control.Monad (ap)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Classes
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Test.QuickCheck.Monadic (monadicIO)
import qualified Data.Foldable as F
#endif

-- | A set of laws associated with a typeclass.
data Laws = Laws
  { lawsTypeclass :: String
    -- ^ Name of the typeclass whose laws are tested
  , lawsProperties :: [(String,Property)]
    -- ^ Pairs of law name and property
  }

-- | A convenience function for working testing properties in GHCi.
--   See the test suite of this library for an example of how to
--   integrate multiple properties into larger test suite.
lawsCheck :: Laws -> IO ()
lawsCheck (Laws className properties) = do
  flip foldMapA properties $ \(name,p) -> do
    putStr (className ++ ": " ++ name ++ " ")
    quickCheck p

-- | A convenience function for checking multiple typeclass instances
--   of multiple types.
lawsCheckMany ::
     [(String,[Laws])] -- ^ Element is type name paired with typeclass laws
  -> IO ()
lawsCheckMany xs = do
  putStrLn "Testing properties for common typeclasses"
  r <- flip foldMapA xs $ \(typeName,laws) -> do
    putStrLn $ "------------"
    putStrLn $ "-- " ++ typeName
    putStrLn $ "------------"
    flip foldMapA laws $ \(Laws typeClassName properties) -> do
      flip foldMapA properties $ \(name,p) -> do
        putStr (typeClassName ++ ": " ++ name ++ " ")
        r <- quickCheckResult p
        return $ case r of
          Success _ _ _ -> Good
          _ -> Bad
  putStrLn ""
  case r of
    Good -> putStrLn "All tests succeeded"
    Bad -> putStrLn "One or more tests failed"

data Status = Bad | Good

instance Monoid Status where
  mempty = Good
  mappend Good x = x
  mappend Bad _ = Bad

newtype Ap f a = Ap { getAp :: f a }

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  {-# INLINE mempty #-}
  mempty = Ap $ pure mempty
  {-# INLINE mappend #-}
  mappend (Ap x) (Ap y) = Ap $ liftA2 mappend x y

foldMapA :: (Foldable t, Monoid m, Applicative f) => (a -> f m) -> t a -> f m
foldMapA f = getAp . foldMap (Ap . f)

-- | Tests the following properties:
-- -- [/Partial Isomorphism/]
--   @decode . encode ≡ Just@
-- [/Encoding Equals Value/]
--   @decode . encode ≡ Just . toJSON@
--
-- Note that in the second propertiy, the type of decode is @ByteString -> Value@,
-- not @ByteString -> a@
jsonLaws :: With [ToJSON, FromJSON, Show, Arbitrary, Eq] a => Proxy a -> Laws
jsonLaws p = Laws "ToJSON/FromJSON"
  [ ("Partial Isomorphism", jsonEncodingPartialIsomorphism p)
  , ("Encoding Equals Value", jsonEncodingEqualsValue p)
  ]

-- | Tests the following properties:
--
-- [/Partial Isomorphism/]
--   @fromList . toList ≡ id@
-- [/Length Preservation/]
--   @fromList xs ≡ fromListN (length xs) xs@
isListLaws :: With [IsList, Show, Arbitrary, Eq] a => With [Show, Arbitrary] (Item a) => Proxy a -> Laws
isListLaws p = Laws "IsList"
  [ ("Partial Isomorphism", isListPartialIsomorphism p)
  , ("Length Preservation", isListLengthPreservation p)
  ]

showReadLaws :: With [Show, Read, Eq, Arbitrary] a => Proxy a -> Laws
showReadLaws p = Laws "Show/Read"
  [ ("Partial Isomorphism", showReadPartialIsomorphism p)
  ]

-- | Tests the following properties:
--
-- [/Associative/]
--   @a <> (b <> c) ≡ (a <> b) <> c@
semigroupLaws :: With [Semigroup, Eq, Arbitrary, Show] a => Proxy a -> Laws
semigroupLaws p = Laws "Semigroup"
  [ ("Associative", semigroupAssociative p)
  ]

-- | Tests the following properties:
--
-- [/Transitive/]
--   @a == b ∧ b == c ⇒ a == c@
-- [/Symmetric/]
--   @a == b ⇒ b == a@
-- [/Reflexive/]
--   @a == a@
--
-- Some of these properties involve implication. In the case that
-- the left hand side of the implication arrow does not hold, we
-- do not retry. Consequently, these properties only end up being
-- useful when the data type has a small number of inhabitants.
eqLaws :: With [Eq, Arbitrary, Show] a => Proxy a -> Laws
eqLaws p = Laws "Eq"
  [ ("Transitive", eqTransitive p)
  , ("Symmetric", eqSymmetric p)
  , ("Reflexive", eqReflexive p)
  ]

-- | Tests the following properties:
--
-- [/Transitive/]
--   @a ≤ b ∧ b ≤ c ⇒ a ≤ c@
-- [/Comparable/]
--   @a ≤ b ∨ a > b@
ordLaws :: With [Ord, Arbitrary, Show] a => Proxy a -> Laws
ordLaws p = Laws "Ord"
  [ ("Transitive", ordTransitive p)
  , ("Comparable", ordComparable p)
  ]

-- | Tests the following properties:
--
-- [/Associative/]
--   @mappend a (mappend b c) ≡ mappend (mappend a b) c@
-- [/Left Identity/]
--   @mappend mempty a ≡ a@
-- [/Right Identity/]
--   @mappend a mempty ≡ a@
monoidLaws :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
monoidLaws p = Laws "Monoid"
  [ ("Associative", monoidAssociative p)
  , ("Left Identity", monoidLeftIdentity p)
  , ("Right Identity", monoidRightIdentity p)
  ]

-- | Tests everything from 'monoidProps' plus the following:
--
-- [/Commutative/]
--   @mappend a b ≡ mappend b a@
commutativeMonoidLaws :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
commutativeMonoidLaws p = Laws "Commutative Monoid" $ lawsProperties (monoidLaws p) ++
  [ ("Commutative", monoidCommutative p)
  ] 
-- | Tests the following properties:
--
-- [/Quotient Remainder/]
--   @(quot x y) * y + (rem x y) ≡ x@
-- [/Division Modulus/]
--   @(div x y) * y + (mod x y) ≡ x@
-- [/Integer Roundtrip/]
--   @fromInteger (toInteger x) ≡ x@
integralLaws :: (Integral a, Arbitrary a, Show a) => Proxy a -> Laws
integralLaws p = Laws "Monoid"
  [ ("Quotient Remainder", integralQuotientRemainder p)
  , ("Division Modulus", integralDivisionModulus p)
  , ("Integer Roundtrip", integralIntegerRoundtrip p)
  ]

-- | Tests the following properties:
--
-- [/Conjunction Idempotence/]
--   @n .&. n ≡ n@
-- [/Disjunction Idempotence/]
--   @n .|. n ≡ n@
-- [/Double Complement/]
--   @complement (complement n) ≡ n@
-- [/Set Bit/]
--   @setBit n i ≡ n .|. bit i@
-- [/Clear Bit/]
--   @clearBit n i ≡ n .&. complement (bit i)@
-- [/Complement Bit/]
--   @complementBit n i ≡ xor n (bit i)@
-- [/Clear Zero/]
--   @clearBit zeroBits i ≡ zeroBits@
-- [/Set Zero/]
--   @setBit zeroBits i ≡ bit i@
-- [/Test Zero/]
--   @testBit zeroBits i ≡ False@
-- [/Pop Zero/]
--   @popCount zeroBits ≡ 0@
-- [/Count Leading Zeros of Zero/]
--   @countLeadingZeros zeroBits ≡ finiteBitSize ⊥@
-- [/Count Trailing Zeros of Zero/]
--   @countTrailingZeros zeroBits ≡ finiteBitSize ⊥@
--
-- All of the useful instances of the 'Bits' typeclass
-- also have 'FiniteBits' instances, so these property
-- tests actually require that instance as well.
bitsLaws :: (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Laws
bitsLaws p = Laws "Bits"
  [ ("Conjunction Idempotence", bitsConjunctionIdempotence p)
  , ("Disjunction Idempotence", bitsDisjunctionIdempotence p)
  , ("Double Complement", bitsDoubleComplement p)
  , ("Set Bit", bitsSetBit p)
  , ("Clear Bit", bitsClearBit p)
  , ("Complement Bit", bitsComplementBit p)
  , ("Clear Zero", bitsClearZero p)
  , ("Set Zero", bitsSetZero p)
  , ("Test Zero", bitsTestZero p)
  , ("Pop Zero", bitsPopZero p)
  , ("Count Leading Zeros of Zero", bitsCountLeadingZeros p)
  , ("Count Trailing Zeros of Zero", bitsCountTrailingZeros p)
  ]

-- | Test that a 'Prim' instance obey the several laws.
primLaws :: (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
primLaws p = Laws "Prim"
  [ ("ByteArray Set-Get (you get back what you put in)", primSetGetByteArray p)
  , ("ByteArray Get-Set (putting back what you got out has no effect)", primGetSetByteArray p)
  , ("ByteArray Set-Set (setting twice is same as setting once)", primSetSetByteArray p)
  , ("ByteArray List Conversion Roundtrips", primListByteArray p)
  , ("Addr Set-Get (you get back what you put in)", primSetGetAddr p)
  , ("Addr Get-Set (putting back what you got out has no effect)", primGetSetAddr p)
  , ("Addr List Conversion Roundtrips", primListAddr p)
  ]

storableLaws :: (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
storableLaws p = Laws "Storable"
  [ ("Set-Get (you get back what you put in)", storableSetGet p)
  , ("Get-Set (putting back what you got out has no effect)", storableGetSet p)
  , ("List Conversion Roundtrips", storableList p)
  ]

isListPartialIsomorphism :: forall a. (IsList a, Show a, Arbitrary a, Eq a) => Proxy a -> Property
isListPartialIsomorphism _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "fromList (toList a)"
  (\a -> fromList (toList a))
  "a"
  (\a -> a)

isListLengthPreservation :: forall a. (IsList a, Show (Item a), Arbitrary (Item a), Eq a) => Proxy a -> Property
isListLengthPreservation _ = property $ \(xs :: [Item a]) ->
  (fromList xs :: a) == fromListN (length xs) xs

showReadPartialIsomorphism :: forall a. (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadPartialIsomorphism _ = property $ \(a :: a) ->
  readMaybe (show a) == Just a

-- TODO: improve the quality of the error message if
-- something does not pass this test.
jsonEncodingEqualsValue :: forall a. (ToJSON a, Show a, Arbitrary a) => Proxy a -> Property
jsonEncodingEqualsValue _ = property $ \(a :: a) ->
  case AE.decode (AE.encode a) of
    Nothing -> False
    Just (v :: AE.Value) -> v == toJSON a

jsonEncodingPartialIsomorphism :: forall a. (ToJSON a, FromJSON a, Show a, Eq a, Arbitrary a) => Proxy a -> Property
jsonEncodingPartialIsomorphism _ = property $ \(a :: a) ->
  AE.decode (AE.encode a) == Just a

eqTransitive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqTransitive _ = property $ \(a :: a) b c -> case a == b of
  True -> case b == c of
    True -> a == c
    False -> a /= c
  False -> case b == c of
    True -> a /= c
    False -> True

-- Technically, this tests something a little stronger than it is supposed to.
-- But that should be alright since this additional strength is implied by
-- the rest of the Ord laws.
ordTransitive :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordTransitive _ = property $ \(a :: a) b c -> case (compare a b, compare b c) of
  (LT,LT) -> a < c
  (LT,EQ) -> a < c
  (LT,GT) -> True
  (EQ,LT) -> a < c
  (EQ,EQ) -> a == c
  (EQ,GT) -> a > c
  (GT,LT) -> True
  (GT,EQ) -> a > c
  (GT,GT) -> a > c

ordComparable :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordComparable _ = property $ \(a :: a) b -> a > b || b >= a

eqSymmetric :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqSymmetric _ = property $ \(a :: a) b -> case a == b of
  True -> b == a
  False -> b /= a

eqReflexive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqReflexive _ = property $ \(a :: a) -> a == a

semigroupAssociative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupAssociative _ = property $ \(a :: a) b c -> a SG.<> (b SG.<> c) == (a SG.<> b) SG.<> c

monoidAssociative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidAssociative _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "mappend a (mappend b c)"
  (\(a,b,c) -> mappend a (mappend b c))
  "mappend (mappend a b) c"
  (\(a,b,c) -> mappend (mappend a b) c)

monoidLeftIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidLeftIdentity _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "mappend mempty a"
  (\a -> mappend mempty a)
  "a"
  (\a -> a)

monoidRightIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidRightIdentity _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "mappend a mempty"
  (\a -> mappend a mempty)
  "a"
  (\a -> a)

bitsConjunctionIdempotence :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsConjunctionIdempotence _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "n .&. n"
  (\n -> n .&. n)
  "n"
  (\n -> n)

bitsDisjunctionIdempotence :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsDisjunctionIdempotence _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "n .|. n"
  (\n -> n .|. n)
  "n"
  (\n -> n)

bitsDoubleComplement :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsDoubleComplement _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "complement (complement n)"
  (\n -> complement (complement n))
  "n"
  (\n -> n)

bitsSetBit :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsSetBit _ = myForAllShrink True (const True)
  (\(n :: a, BitIndex i :: BitIndex a) -> ["n = " ++ show n, "i = " ++ show i])
  "setBit n i"
  (\(n,BitIndex i) -> setBit n i)
  "n .|. bit i"
  (\(n,BitIndex i) -> n .|. bit i)

bitsClearBit :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsClearBit _ = myForAllShrink True (const True)
  (\(n :: a, BitIndex i :: BitIndex a) -> ["n = " ++ show n, "i = " ++ show i])
  "clearBit n i"
  (\(n,BitIndex i) -> clearBit n i)
  "n .&. complement (bit i)"
  (\(n,BitIndex i) -> n .&. complement (bit i))

bitsComplementBit :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsComplementBit _ = myForAllShrink True (const True)
  (\(n :: a, BitIndex i :: BitIndex a) -> ["n = " ++ show n, "i = " ++ show i])
  "complementBit n i"
  (\(n,BitIndex i) -> complementBit n i)
  "xor n (bit i)"
  (\(n,BitIndex i) -> xor n (bit i))

bitsClearZero :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsClearZero _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "complement (complement n)"
  (\n -> complement (complement n))
  "n"
  (\n -> n)

bitsSetZero :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsSetZero _ = myForAllShrink True (const True)
  (\(BitIndex i :: BitIndex a) -> ["i = " ++ show i])
  "setBit zeroBits i"
  (\(BitIndex i) -> setBit (zeroBits :: a) i)
  "bit i"
  (\(BitIndex i) -> bit i)

bitsTestZero :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsTestZero _ = myForAllShrink True (const True)
  (\(BitIndex i :: BitIndex a) -> ["i = " ++ show i])
  "testBit zeroBits i"
  (\(BitIndex i) -> testBit (zeroBits :: a) i)
  "False"
  (\_ -> False)

bitsPopZero :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsPopZero _ = myForAllShrink True (const True)
  (\() -> [])
  "popCount zeroBits"
  (\() -> popCount (zeroBits :: a))
  "0"
  (\() -> 0)

bitsCountLeadingZeros :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsCountLeadingZeros _ = myForAllShrink True (const True)
  (\() -> [])
  "countLeadingZeros zeroBits"
  (\() -> countLeadingZeros (zeroBits :: a))
  "finiteBitSize undefined"
  (\() -> finiteBitSize (undefined :: a))

bitsCountTrailingZeros :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsCountTrailingZeros _ = myForAllShrink True (const True)
  (\() -> [])
  "countTrailingZeros zeroBits"
  (\() -> countTrailingZeros (zeroBits :: a))
  "finiteBitSize undefined"
  (\() -> finiteBitSize (undefined :: a))

integralQuotientRemainder :: forall a. (Integral a, Arbitrary a, Show a) => Proxy a -> Property
integralQuotientRemainder _ = myForAllShrink False (\(_,y) -> y /= 0)
  (\(x :: a, y) -> ["x = " ++ show x, "y = " ++ show y])
  "(quot x y) * y + (rem x y)"
  (\(x,y) -> (quot x y) * y + (rem x y))
  "x"
  (\(x,_) -> x)

integralDivisionModulus :: forall a. (Integral a, Arbitrary a, Show a) => Proxy a -> Property
integralDivisionModulus _ = myForAllShrink False (\(_,y) -> y /= 0)
  (\(x :: a, y) -> ["x = " ++ show x, "y = " ++ show y])
  "(div x y) * y + (mod x y)"
  (\(x,y) -> (div x y) * y + (mod x y))
  "x"
  (\(x,_) -> x)

integralIntegerRoundtrip :: forall a. (Integral a, Arbitrary a, Show a) => Proxy a -> Property
integralIntegerRoundtrip _ = myForAllShrink False (const True)
  (\(x :: a) -> ["x = " ++ show x])
  "fromInteger (toInteger x)"
  (\x -> fromInteger (toInteger x))
  "x"
  (\x -> x)

monoidCommutative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidCommutative _ = myForAllShrink True (const True)
  (\(a :: a,b) -> ["a = " ++ show a, "b = " ++ show b])
  "mappend a b"
  (\(a,b) -> mappend a b)
  "mappend b a"
  (\(a,b) -> mappend b a)

primListByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListByteArray _ = property $ \(as :: [a]) ->
  as == toList (fromList as :: PrimArray a)

primListAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListAddr _ = property $ \(as :: [a]) -> unsafePerformIO $ do
  let len = L.length as
  ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
  let addr = Addr addr#
  let go :: Int -> [a] -> IO ()
      go !ix xs = case xs of
        [] -> return ()
        (x : xsNext) -> do
          writeOffAddr addr ix x
          go (ix + 1) xsNext
  go 0 as
  let rebuild :: Int -> IO [a]
      rebuild !ix = if ix < len
        then (:) <$> readOffAddr addr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as == asNew)

primSetGetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetGetByteArray _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ runST $ do
    arr <- newPrimArray len
    writePrimArray arr ix a
    a' <- readPrimArray arr ix
    return (a == a')

primGetSetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetSetByteArray _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = fromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  arr2 <- return $ runST $ do
    marr <- newPrimArray len
    copyPrimArray marr 0 arr1 0 len
    a <- readPrimArray marr ix
    writePrimArray marr ix a
    unsafeFreezePrimArray marr
  return (arr1 == arr2)

primSetSetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetSetByteArray _ = property $ \(a :: a) (as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = fromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  (arr2,arr3) <- return $ runST $ do
    marr2 <- newPrimArray len
    copyPrimArray marr2 0 arr1 0 len
    writePrimArray marr2 ix a
    marr3 <- newPrimArray len
    copyMutablePrimArray marr3 0 marr2 0 len
    arr2 <- unsafeFreezePrimArray marr2
    writePrimArray marr3 ix a
    arr3 <- unsafeFreezePrimArray marr3
    return (arr2,arr3)
  return (arr2 == arr3)

primSetGetAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetGetAddr _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
    let addr = Addr addr#
    writeOffAddr addr ix a
    a' <- readOffAddr addr ix
    free ptr
    return (a == a')

primGetSetAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetSetAddr _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = fromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  arr2 <- return $ unsafePerformIO $ do
    ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
    let addr = Addr addr#
    copyPrimArrayToPtr ptr arr1 0 len
    a :: a <- readOffAddr addr ix
    writeOffAddr addr ix a
    marr <- newPrimArray len
    copyPtrToMutablePrimArray marr 0 ptr len
    free ptr
    unsafeFreezePrimArray marr
  return (arr1 == arr2)

storableSetGet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableSetGet _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptr :: Ptr a <- mallocArray len
    pokeElemOff ptr ix a
    a' <- peekElemOff ptr ix
    free ptr
    return (a == a')

storableGetSet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableGetSet _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let len = L.length as
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptrA <- newArray as
    ptrB <- mallocArray len
    copyArray ptrB ptrA len
    a <- peekElemOff ptrA ix
    pokeElemOff ptrA ix a
    res <- arrayEq ptrA ptrB len
    free ptrA
    free ptrB
    return res

storableList :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableList _ = property $ \(as :: [a]) -> unsafePerformIO $ do
  let len = L.length as
  ptr <- newArray as
  let rebuild :: Int -> IO [a]
      rebuild !ix = if ix < len
        then (:) <$> peekElemOff ptr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as == asNew)

arrayEq :: forall a. (Storable a, Eq a) => Ptr a -> Ptr a -> Int -> IO Bool
arrayEq ptrA ptrB len = go 0 where
  go !i = if i < len
    then do
      a <- peekElemOff ptrA i
      b <- peekElemOff ptrB i
      if a == b
        then go (i + 1)
        else return False
    else return True

#if MIN_VERSION_QuickCheck(2,10,0)
-- | Tests the following applicative properties:
--
-- [/Identity/]
--   @'fmap' 'id' ≡ 'id'@
-- [/Composition/]
--   @fmap (f . g) ≡ 'fmap' f . 'fmap' g@
-- [/Const/]
--   @(<$) ≡ 'fmap' 'const'@
functorLaws :: (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
functorLaws p = Laws "Functor"
  [ ("Identity", functorIdentity p)
  , ("Composition", functorComposition p)
  , ("Const", functorConst p)
  ]

-- | Tests the following applicative properties:
--
-- [/Identity/]
--   @'pure' 'id' '<*>' v ≡ v@
-- [/Composition/]
--   @'pure' (.) '<*>' u '<*>' v '<*>' w ≡ u '<*>' (v '<*>' w)@
-- [/Homomorphism/]
--   @'pure' f '<*>' 'pure' x ≡ 'pure' (f x)@
-- [/Interchange/]
--   @u '<*>' 'pure' y ≡ 'pure' ('$' y) '<*>' u@
-- [/LiftA2 (1)/]
--   @('<*>') ≡ 'liftA2' 'id'@
applicativeLaws :: (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
applicativeLaws p = Laws "Applicative"
  [ ("Identity", applicativeIdentity p)
  , ("Composition", applicativeComposition p)
  , ("Homomorphism", applicativeHomomorphism p)
  , ("Interchange", applicativeInterchange p)
  , ("LiftA2 Part 1", applicativeLiftA2_1 p)
    -- todo: liftA2 part 2, we need an equation of two variables for this
  ]


-- | Tests the following monadic properties:
--
-- [/Left Identity/]
--   @'return' a '>>=' k ≡ k a@
-- [/Right Identity/]
--   @m '>>=' 'return' ≡ m@
-- [/Associativity/]
--   @m '>>=' (\\x -> k x '>>=' h) ≡ (m '>>=' k) '>>=' h@
-- [/Return/]
--   @'pure' ≡ 'return'@
-- [/Ap/]
--   @('<*>') ≡ 'ap'@
monadLaws :: (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
monadLaws p = Laws "Monad"
  [ ("Left Identity", monadLeftIdentity p)
  , ("Right Identity", monadRightIdentity p)
  , ("Associativity", monadAssociativity p)
  , ("Return", monadReturn p)
  , ("Ap", monadAp p)
  ]

-- | Tests the following 'Foldable' properties:
--
-- [/fold/]
--   @'fold' ≡ 'foldMap' 'id'@
-- [/foldMap/]
--   @'foldMap' f ≡ 'foldr' ('mappend' . f) 'mempty'@
-- [/foldr/]
--   @'foldr' f z t ≡ 'appEndo' ('foldMap' ('Endo' . f) t ) z@
-- [/foldr'/]
--   @'foldr'' f z0 xs = let f\' k x z = k '$!' f x z in 'foldl' f\' 'id' xs z0@
-- [/foldl/]
--   @'foldl' f z t ≡ 'appEndo' ('getDual' ('foldMap' ('Dual' . 'Endo' . 'flip' f) t)) z@
-- [/foldl'/]
--   @'foldl'' f z0 xs = let f' x k z = k '$!' f z x in 'foldr' f\' 'id' xs z0@
-- [/toList/]
--   @'F.toList' ≡ 'foldr' (:) []@
-- [/null/]
--   @'null' ≡ 'foldr' ('const' ('const' 'False')) 'True'@
-- [/length/]
--   @'length' ≡ getSum . foldMap ('const' ('Sum' 1))@
--
-- Note that this checks to ensure that @foldl\'@ and @foldr\'@
-- are suitably strict.
foldableLaws :: (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
foldableLaws = foldableLawsInternal

foldableLawsInternal :: forall f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
foldableLawsInternal p = Laws "Foldable"
  [ (,) "fold" $ property $ \(Apply (a :: f (SG.Sum Integer))) ->
      F.fold a == F.foldMap id a
  , (,) "foldMap" $ property $ \(Apply (a :: f Integer)) (e :: Equation) ->
      let f = SG.Sum . runEquation e
       in foldMap f a == foldr (mappend . f) mempty a
  , (,) "foldr" $ property $ \(e :: EquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runEquationTwo e
       in foldr f z t == SG.appEndo (foldMap (SG.Endo . f) t) z
  , (,) "foldr'" (foldableFoldr' p)
  , (,) "foldl" $ property $ \(e :: EquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runEquationTwo e
       in foldl f z t == SG.appEndo (SG.getDual (foldMap (SG.Dual . SG.Endo . flip f) t)) z
  , (,) "foldl'" (foldableFoldl' p)
  , (,) "toList" $ property $ \(Apply (t :: f Integer)) ->
      eq1 (F.toList t) (foldr (:) [] t)
  , (,) "null" $ property $ \(Apply (t :: f Integer)) ->
      null t == foldr (const (const False)) True t
  , (,) "length" $ property $ \(Apply (t :: f Integer)) ->
      length t == SG.getSum (foldMap (const (SG.Sum 1)) t)
  ]

foldableFoldl' :: forall f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
foldableFoldl' _ = property $ \(_ :: ChooseSecond) (_ :: LastNothing) (Apply (xs :: f (Bottom Integer))) ->
  monadicIO $ do
    let f :: Integer -> Bottom Integer -> Integer
        f a b = case b of
          BottomUndefined -> error "foldableFoldl' example"
          BottomValue v -> if even v
            then a
            else v
        z0 = 0
    r1 <- lift $ do
      let f' x k z = k $! f z x
      e <- try (evaluate (F.foldr f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (F.foldl' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

foldableFoldr' :: forall f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
foldableFoldr' _ = property $ \(_ :: ChooseFirst) (_ :: LastNothing) (Apply (xs :: f (Bottom Integer))) ->
  monadicIO $ do
    let f :: Bottom Integer -> Integer -> Integer
        f a b = case a of
          BottomUndefined -> error "foldableFoldl' example"
          BottomValue v -> if even v
            then v
            else b
        z0 = 0
    r1 <- lift $ do
      let f' k x z = k $! f x z
      e <- try (evaluate (F.foldl f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (F.foldr' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

data ChooseSecond = ChooseSecond
  deriving (Eq)

data ChooseFirst = ChooseFirst
  deriving (Eq)

data LastNothing = LastNothing
  deriving (Eq)

data Bottom a = BottomUndefined | BottomValue a
  deriving (Eq)

instance Show ChooseFirst where
  show ChooseFirst = "\\a b -> if even a then a else b"

instance Show ChooseSecond where
  show ChooseSecond = "\\a b -> if even b then a else b"

instance Show LastNothing where
  show LastNothing = "0"

instance Show a => Show (Bottom a) where
  show x = case x of
    BottomUndefined -> "undefined"
    BottomValue a -> show a

instance Arbitrary ChooseSecond where
  arbitrary = pure ChooseSecond

instance Arbitrary ChooseFirst where
  arbitrary = pure ChooseFirst

instance Arbitrary LastNothing where
  arbitrary = pure LastNothing

instance Arbitrary a => Arbitrary (Bottom a) where
  arbitrary = fmap maybeToBottom arbitrary
  shrink x = map maybeToBottom (shrink (bottomToMaybe x))

bottomToMaybe :: Bottom a -> Maybe a
bottomToMaybe BottomUndefined = Nothing
bottomToMaybe (BottomValue a) = Just a

maybeToBottom :: Maybe a -> Bottom a
maybeToBottom Nothing = BottomUndefined
maybeToBottom (Just a) = BottomValue a

data Apply f a = Apply { getApply :: f a }

instance (Eq1 f, Eq a) => Eq (Apply f a) where
  Apply a == Apply b = eq1 a b

data LinearEquation = LinearEquation
  { _linearEquationLinear :: Integer
  , _linearEquationConstant :: Integer
  } deriving (Eq)

data LinearEquationM m = LinearEquationM (m LinearEquation) (m LinearEquation)

runLinearEquation :: Integer -> LinearEquation -> Integer
runLinearEquation x (LinearEquation a b) = a * x + b

runLinearEquationM :: Functor m => LinearEquationM m -> Integer -> m Integer
runLinearEquationM (LinearEquationM e1 e2) i = if odd i
  then fmap (runLinearEquation i) e1
  else fmap (runLinearEquation i) e2

instance Eq1 m => Eq (LinearEquationM m) where
  LinearEquationM a1 b1 == LinearEquationM a2 b2 = eq1 a1 a2 && eq1 b1 b2

showLinear :: Int -> LinearEquation -> ShowS
showLinear _ (LinearEquation a b) = shows a . showString " * x + " . shows b

showLinearList :: [LinearEquation] -> ShowS
showLinearList xs = SG.appEndo $ mconcat
   $ [SG.Endo (showChar '[')]
  ++ L.intersperse (SG.Endo (showChar ',')) (map (SG.Endo . showLinear 0) xs)
  ++ [SG.Endo (showChar ']')]

instance Show1 m => Show (LinearEquationM m) where
  show (LinearEquationM a b) = (\f -> f "")
    $ showString "\\x -> if odd x then "
    . liftShowsPrec showLinear showLinearList 0 a
    . showString " else "
    . liftShowsPrec showLinear showLinearList 0 b

instance Arbitrary1 m => Arbitrary (LinearEquationM m) where
  arbitrary = liftA2 LinearEquationM arbitrary1 arbitrary1
  shrink (LinearEquationM a b) = concat
    [ map (\x -> LinearEquationM x b) (shrink1 a)
    , map (\x -> LinearEquationM a x) (shrink1 b)
    ]

instance Arbitrary LinearEquation where
  arbitrary = do
    (a,b) <- arbitrary
    return (LinearEquation (abs a) (abs b))
  shrink (LinearEquation a b) =
    let xs = shrink (a,b)
     in map (\(x,y) -> LinearEquation (abs x) (abs y)) xs

-- this is a quadratic equation
data Equation = Equation Integer Integer Integer
  deriving (Eq)

-- This show instance is does not actually provide a
-- way to create an equation. Instead, it makes it look
-- like a lambda.
instance Show Equation where
  show (Equation a b c) = "\\x -> " ++ show a ++ " * x ^ 2 + " ++ show b ++ " * x + " ++ show c

instance Arbitrary Equation where
  arbitrary = do
    (a,b,c) <- arbitrary
    return (Equation (abs a) (abs b) (abs c))
  shrink (Equation a b c) =
    let xs = shrink (a,b,c)
     in map (\(x,y,z) -> Equation (abs x) (abs y) (abs z)) xs

runEquation :: Equation -> Integer -> Integer
runEquation (Equation a b c) x = a * x ^ (2 :: Integer) + b * x + c

-- linear equation of two variables
data EquationTwo = EquationTwo Integer Integer
  deriving (Eq)

-- This show instance is does not actually provide a
-- way to create an EquationTwo. Instead, it makes it look
-- like a lambda that takes two variables.
instance Show EquationTwo where
  show (EquationTwo a b) = "\\x y -> " ++ show a ++ " * x + " ++ show b ++ " * y"

instance Arbitrary EquationTwo where
  arbitrary = do
    (a,b) <- arbitrary
    return (EquationTwo (abs a) (abs b))
  shrink (EquationTwo a b) =
    let xs = shrink (a,b)
     in map (\(x,y) -> EquationTwo (abs x) (abs y)) xs

runEquationTwo :: EquationTwo -> Integer -> Integer -> Integer
runEquationTwo (EquationTwo a b) x y = a * x + b * y

-- This show instance is intentionally a little bit wrong.
-- We don't wrap the result in Apply since the end user
-- should not be made aware of the Apply wrapper anyway.
instance (Show1 f, Show a) => Show (Apply f a) where
  showsPrec p = showsPrec1 p . getApply

instance (Arbitrary1 f, Arbitrary a) => Arbitrary (Apply f a) where
  arbitrary = fmap Apply arbitrary1
  shrink = map Apply . shrink1 . getApply

functorIdentity :: forall f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
functorIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (fmap id a) a

func1 :: Integer -> (Integer,Integer)
func1 i = (div (i + 5) 3, i * i - 2 * i + 1)

func2 :: (Integer,Integer) -> (Bool,Either Ordering Integer)
func2 (a,b) = (odd a, if even a then Left (compare a b) else Right (b + 2))

functorComposition :: forall f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
functorComposition _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap func2 (fmap func1 a)) (fmap (func2 . func1) a)

functorConst :: forall f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
functorConst _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap (const 'X') a) ('X' <$ a)

applicativeIdentity :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (pure id <*> a) a

applicativeComposition :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeComposition _ = property $ \(Apply (u' :: f Equation)) (Apply (v' :: f Equation)) (Apply (w :: f Integer)) ->
  let u = fmap runEquation u'
      v = fmap runEquation v'
   in eq1 (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

applicativeHomomorphism :: forall f. (Applicative f, Eq1 f, Show1 f) => Proxy f -> Property
applicativeHomomorphism _ = property $ \(e :: Equation) (a :: Integer) ->
  let f = runEquation e
   in eq1 (pure f <*> pure a) (pure (f a) :: f Integer)

applicativeInterchange :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeInterchange _ = property $ \(Apply (u' :: f Equation)) (y :: Integer) ->
  let u = fmap runEquation u'
   in eq1 (u <*> pure y) (pure ($ y) <*> u)

applicativeLiftA2_1 :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeLiftA2_1 _ = property $ \(Apply (f' :: f Equation)) (Apply (x :: f Integer)) -> 
  let f = fmap runEquation f'
   in eq1 (liftA2 id f x) (f <*> x)

monadLeftIdentity :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadLeftIdentity _ = property $ \(k' :: LinearEquationM f) (a :: Integer) -> 
  let k = runLinearEquationM k'
   in eq1 (return a >>= k) (k a)

monadRightIdentity :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadRightIdentity _ = property $ \(Apply (m :: f Integer)) -> 
  eq1 (m >>= return) m

monadAssociativity :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadAssociativity _ = property $ \(Apply (m :: f Integer)) (k' :: LinearEquationM f) (h' :: LinearEquationM f) -> 
  let k = runLinearEquationM k'
      h = runLinearEquationM h'
   in eq1 (m >>= (\x -> k x >>= h)) ((m >>= k) >>= h)

monadReturn :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadReturn _ = property $ \(x :: Integer) ->
  eq1 (return x) (pure x :: f Integer)

monadAp :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadAp _ = property $ \(Apply (f' :: f Equation)) (Apply (x :: f Integer)) -> 
  let f = fmap runEquation f'
   in eq1 (ap f x) (f <*> x)

#endif

myForAllShrink :: (Arbitrary a, Show b, Eq b) => Bool -> (a -> Bool) -> (a -> [String]) -> String -> (a -> b) -> String -> (a -> b) -> Property
myForAllShrink displayRhs isValid showInputs name1 calc1 name2 calc2 =
  again $
  MkProperty $
  arbitrary >>= \x ->
    unProperty $
    shrinking shrink x $ \x' ->
      let b1 = calc1 x'
          b2 = calc2 x'
          sb1 = show b1
          sb2 = show b2
          description = "  Description: " ++ name1 ++ " = " ++ name2
          err = description ++ "\n" ++ unlines (map ("  " ++) (showInputs x')) ++ "  " ++ name1 ++ " = " ++ sb1 ++ (if displayRhs then "\n  " ++ name2 ++ " = " ++ sb2 else "")
       in isValid x' ==> counterexample err (b1 == b2)

newtype BitIndex a = BitIndex Int

instance FiniteBits a => Arbitrary (BitIndex a) where
  arbitrary = let n = finiteBitSize (undefined :: a) in if n > 0
    then fmap BitIndex (choose (0,n - 1))
    else return (BitIndex 0)
  shrink (BitIndex x) = if x > 0 then map BitIndex (S.toList (S.fromList [x - 1, div x 2, 0])) else []

