{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq  :: Expr Int -> Expr Int -> Expr Bool
  -- exercise: allow comparing booleans, e.g `Eq (B True) (B True)`
  
eval :: Expr a -> a
eval (I n)       = n
eval (B b)       = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

deriving instance Show (Expr a)
