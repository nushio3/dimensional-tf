{-# LANGUAGE DataKinds, TypeOperators #-}
import GHC.TypeLits



main = do
  print $ (sing :: Sing 1)
  print $ (sing :: Sing "moge")
--  print $ (sing :: Sing 6) * 7
  print $ fromSing (sing :: Sing 6) * 7
  print $ map fromSing [sing :: Sing (6*7),sing :: Sing 42]
{-
$ runhaskell compute-01.hs

compute-01.hs:11:25:
    Couldn't match type `6 * 7' with `42'
    Expected type: Sing Nat (6 * 7)
      Actual type: Sing Nat 42
    In the expression: sing :: Sing (6 * 7)
    In the second argument of `map', namely
      `[sing :: Sing (6 * 7), sing :: Sing 42]'
    In the second argument of `($)', namely
      `map fromSing [sing :: Sing (6 * 7), sing :: Sing 42]'

-}