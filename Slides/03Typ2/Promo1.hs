{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
module Promo1 where

data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat

-- This defines
-- Type Nat
-- Value constructors: Z, S

-- Promotion (lifting) to type level yields
-- kind Nat
-- type constructors: 'Z :: Nat; 'S :: Nat -> Nat
-- 's can be omitted in most cases, but...

-- data P          -- 1
-- data Prom = P   -- 2
-- type T = P      -- 1 or promoted 2?
-- quote disambiguates:
-- type T1 = P     -- 1
-- type T2 = 'P    -- promoted 2

-- Other promotions
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList t -> HList (a ': t)

data Tuple :: (*,*) -> * where
  Tuple :: a -> b -> Tuple '(a,b)

foo0 :: HList '[]
foo0 = HNil

foo1 :: HList '[Int]
foo1 = HCons (3::Int) HNil

foo2 :: HList [Int, Bool]
foo2 = undefined  -- (easy) exercise

infixr 6 :>
data Vec :: Nat -> * -> * where
  V0   :: Vec 'Z a
  (:>) :: a -> Vec n a -> Vec ('S n) a

deriving instance (Show a) => Show (Vec n a)


infixl 6 :+

type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z :+ m = m
type instance (S n) :+ m = S (n :+ m)

vhead :: Vec (S n) a -> a
vhead (x:>_) = x

vtail :: Vec (S n) a -> Vec n a
vtail (_:> xs) = xs

vapp :: Vec m a -> Vec n a -> Vec (m :+ n) a
vapp V0 ys = ys
vapp (x:>xs) ys = x:>(vapp xs ys)

-- |
-- Indexing
-- >>> (1:>V0) `atIndex` FinZ
-- 1
--
-- atIndex :: Vec n a -> (m < n) -> a

data Fin n where
    FinZ :: Fin (S n) -- zero is less than any successor
    FinS :: Fin n -> Fin (S n) -- n is less than (n+1)

atIndex :: Vec n a -> Fin n -> a
atIndex (x:>xs) FinZ = x
atIndex (x:>xs) (FinS k) = atIndex xs k

-- Question - why not:
-- atIndex :: Vec (S n) a -> ... ?

-- Want
vreplicate1 :: Nat -> a -> Vec n a
vreplicate1 = undefined
-- vreplicate1 _ = V0 -- doesn't work

-- this does not work either
-- vreplicate2 :: (n::Nat) -> a -> Vec n a


vchop1 :: Vec (m :+ n) a -> (Vec m a, Vec n a)
vchop1 _ = undefined

-- | Chop a vector in two, using first argument as a measure
-- >>> vchop2 (undefined :> V0) (1 :> 2 :> V0)
-- (1 :> V0,2 :> V0)
vchop2 :: Vec m x -> Vec (m :+ n) a -> (Vec m a, Vec n a)
vchop2 V0 xs = (V0, xs)
vchop2 (_:>m) (x:>xs) = (x:>ys, zs) where
  (ys, zs) = vchop2 m xs

-- inhabitants of Nat types
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
deriving instance Show(SNat n)


add :: (SNat m) -> (SNat n) -> SNat(m :+ n)
add SZ n = n
add (SS m) n = SS (add m n)

-- | `vreplicate n a` is a vector of n copies of a
-- >>> vreplicate (SS SZ) 1
-- 1 :> V0
-- >>> vreplicate (SS (SS SZ)) 1
-- 1 :> (1 :> V0)
vreplicate :: SNat n -> a -> Vec n a
vreplicate SZ _ = V0
vreplicate (SS n) x = x:>(vreplicate n x)

-- | chop a vector in two parts
-- >>> vchop (SS SZ) (1 :> 2 :> V0)
-- (1 :> V0,2 :> V0)
vchop = vchop3
vchop3 :: SNat m -> Vec(m:+n) a -> (Vec m a, Vec n a)
vchop3 SZ xs = (V0, xs)
vchop3 (SS m) (x:>xs) = (x:>ys, zs) where
  (ys,zs) = vchop3 m xs

-- Exercise: define multiplication

-- | Take first `n` elements of a vector
vtake1 :: SNat m -> NP n -> Vec (m :+ n) a -> Vec m a
vtake1 = undefined

-- | Nat Proxy
data NP :: Nat -> * where NP :: NP n

vtake2 :: SNat m -> NP n -> Vec (m :+ n) a -> Vec m a
vtake2 SZ     n xs = V0
-- vtake2 (SS m) n

-- | Generic Proxy
data Proxy :: k -> * where
  Proxy :: Proxy i

