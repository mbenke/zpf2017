{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, KindSignatures, GADTs,
             FlexibleInstances, FlexibleContexts, UndecidableInstances,
             RankNTypes, TypeOperators, MultiParamTypeClasses,
             FunctionalDependencies, ScopedTypeVariables,
             LambdaCase, TemplateHaskell, EmptyCase, TypeInType
 #-}

import Data.Singletons
import Data.Singletons.TH
import Data.Proxy
import Data.Kind
import Data.Singletons.SuppressUnusedWarnings

$(singletons [d|
  data Nat where
    Z :: Nat
    S :: Nat -> Nat
      deriving (Eq, Show, Read)

  plus :: Nat -> Nat -> Nat
  plus Z m = m
  plus (S n) m = S (plus n m)

  pred :: Nat -> Nat
  pred Z = Z
  pred (S n) = n
 |])

deriving instance Show (SNat n)
-- deriving instance Eq (SNat n)

infixr 6 :>
data Vec :: Nat -> * -> * where
  V0   :: Vec 'Z a
  (:>) :: a -> Vec n a -> Vec ('S n) a

deriving instance (Show a) => Show (Vec n a)

vhead :: Vec ('S n) a -> a
vhead (x:>_) = x

vtail :: Vec ('S n) a -> Vec n a
vtail (_:> xs) = xs

vapp :: Vec m a -> Vec n a -> Vec (Plus m n) a
vapp V0 ys = ys
vapp (x:>xs) ys = x:>(vapp xs ys)

vreplicate :: SNat n -> a -> Vec n a
vreplicate SZ _ = V0
vreplicate (SS n) x = x:>(vreplicate n x)

-- | chop a vector in two parts
-- >>> vchop (SS SZ) (1 :> 2 :> V0)
-- (1 :> V0,2 :> V0)
vchop :: SNat m -> Vec(Plus m n) a -> (Vec m a, Vec n a)
vchop SZ xs = (V0, xs)
vchop (SS m) (x:>xs) = (x:>ys, zs) where
  (ys,zs) = vchop m xs
