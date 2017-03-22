% Zaawansowane programowanie funkcyjne
% Marcin Benke
% 9 maja 2017

<meta name="duration" content="80" />
# Plan
1. Rodzaje
2. GADT - https://en.wikibooks.org/wiki/Haskell/GADT
3. Promocja typów - https://github.com/slindley/dependent-haskell
    ```
    data Nat = Z | S Nat
    data Vec :: Nat -> * -> * where
    vhead :: Vec (S n) a -> a
    ```

4. Rodziny typów
   ```
   type family (m::Nat) :+ (n::Nat) :: Nat
   vappend :: Vec m a -> Vec n a -> Vec (m :+ n) a
   ? :: Vec(m :+ n) a -> (Vec m a, Vec n a)
   ```

5. Zależności dynamiczne, singletony
   ```
   data Natty :: Nat -> *
   vchop :: Natty m -> Vec (m :+ n) a -> (Vec m a, Vec n a)
   ? :: Natty m -> Vec (m :+ n) a -> Vec m a
   ```

6. Zależności statyczne, Proxy
   ```
   data NP :: Nat -> * where NP :: NP n
   vtake1 :: Natty m -> NP n -> Vec (m :+ n) -> Vec m a
   ```

7. Kind polymorphism
   ```
   data Proxy :: k -> * where Proxy :: Proxy i
   vtake2 :: Natty m -> Proxy n -> Vec (m :+ n) -> Vec m a 
   ```

8. Aplikacja wektorowa
   ```
   type family Arity (n::Nat) (a::*) :: *
   vap :: Arity n a -> Vec n a -> a
   ```
   
9. Zależności implicite
   ```
   class NATTY (n::Nat)
   vtrunc :: NATTY m => Proxy n -> Vec (m :+ n) a -> Vec m a
   ```

10. TypeApplication

11. Biblioteka `singletons`
