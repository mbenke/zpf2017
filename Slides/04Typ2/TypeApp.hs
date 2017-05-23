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
