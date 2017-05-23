# Rodziny typów

``` {.haskell}
type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z :+ m = m
type instance (S n) :+ m = S (n :+ m)

vapp :: Vec m a -> Vec n a -> Vec (m :+ n) a
vapp Vnil ys = ys
vapp (Vcons x xs) ys = Vcons x (vapp xs ys)
```

**Ćwiczenie:** zdefiniować mnożenie
``` {.haskell}
type family (n :: Nat) :* (m :: Nat) :: Nat
```

# Replicate

Spróbujmy stworzyć analog funkcji `replicate :: Int -> a -> [a]`

``` {.haskell}
vreplicate :: Nat -> a -> Vec n a
vreplicate Z _ = Vnil -- fail on oh, so many levels
```

dokładniej

``` {.haskell}
vreplicate2 :: (n::Nat) -> a -> Vec n a
```

...ale nie ma wartości typu rodzaju `Nat`

*Ćwiczenie:* wypróbować różne varianty `vreplicate`

# Singleton
Nie potrzebujemy całego wektora, a tylko jego długości.

Ale typ Nat jest za mało precyzyjny, mamy `Z :: Nat`, chielibyśmy `Zero :: SNat Z` 

Pomysł: stwórzmy po jednym reprezentancie każdego z typów rodzaju Nat

``` {.haskell}
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
deriving instance Show(SNat n)

add :: (SNat m) -> (SNat n) -> SNat(m :+ n)
add SZ n = n
add (SS m) n = SS (add m n)
```
**Ćwiczenie:** zdefiniować mnożenie
``` {.haskell}
mul :: (SNat m) -> (SNat n) -> SNat(m :* n)
```

# vreplicate

``` {.haskell}
-- | `vreplicate n a` is a vector of n copies of a
-- >>> vreplicate (SS SZ) 1
-- Vcons 1 Vnil
-- >>> vreplicate (SS (SS SZ)) 1
-- Vcons 1 (Vcons 1 Vnil)
vreplicate :: SNat n -> a -> Vec n a
vreplicate SZ _ = Vnil
vreplicate (SS n) x = Vcons x (vreplicate n x)
```

**Ćwiczenie:** zdefiniować

``` {.haskell}
vcycle :: SNat n -> Vec m a -> Vec (n:*m) a
```

#  Kind polymorphism
Możemy zdefiniować jedno `Proxy` dla wszystkich rodzajów

```
   data Proxy :: k -> * where Proxy :: Proxy i
   vtake2 :: Natty m -> Proxy n -> Vec (m :+ n) -> Vec m a 
```

Wymaga to jednak roszerzenia `KindPolymorphism`

# Ćwiczenie: aplikacja wektorowa


Chcemy zastosować n-argumentowy operator do wektora n argumentów

```
   type family Arity (n::Nat) (a::*) :: *
   vap :: Arity n a -> Vec n a -> a
-- >>> vap (+) (1 :> 2 :> V0)
-- 3
```

# TypeApplication

```
-- Requires GHC >= 8.0
{-# LANGUAGE TypeApplications, ExplicitForAll, GADTs #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, AllowAmbiguousTypes #-}
import Data.Proxy
-- data Proxy :: k -> * where Proxy :: Proxy i

answer_read = show (read @Int "3") -- "3" :: String
answer_show = show @Integer (read "5") -- "5" :: String
answer_showread = show @Int (read @Int "7") -- "7" :: String

incShow :: forall a . (Read a, Show a, Num a) => String -> String
incShow = show . (+1) . read @a
-- >>> incShow @Int "3"
-- "4"
-- >>> incShow @Double "3.0"
-- "4.0"

incShow7 :: forall a . (Read a, Show a, Num a) => Proxy a -> String -> String
incShow7 _ = show . (+1) . (read :: String -> a)
-- >>> incShow7 (Proxy::Proxy Double) "3.0"
```

** Ćwiczenie: ** przepisać funkcję `vtake` z użyciem aplikacji typowej zamiast `Proxy`

