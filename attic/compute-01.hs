{-# LANGUAGE DataKinds, TypeOperators #-}
import GHC.TypeLits



main = do
  print $ (sing :: Sing 1)
  print $ (sing :: Sing "moge")
--  print $ (sing :: Sing 6) * 7
  print $ fromSing (sing :: Sing 6) * 7
  print $ map fromSing [sing :: Sing (6*7),sing :: Sing 42]
