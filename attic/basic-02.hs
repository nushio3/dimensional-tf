{-# LANGUAGE DataKinds, TypeOperators #-}
import GHC.TypeLits

main = do
  print $ (sing :: Sing 1)
  print $ (sing :: Sing "moge")
--  print $ (sing :: Sing 6) * 7
  print $ fromSing (sing :: Sing 6) * 7
  print $ fromSing (sing :: Sing (6*7))

{-
$ runhaskell basic-02.hs

basic-02.hs:9:21:
    No instance for (SingI Nat (6 * 7)) arising from a use of `sing'
    Possible fix: add an instance declaration for (SingI Nat (6 * 7))
    In the first argument of `fromSing', namely
      `(sing :: Sing (6 * 7))'
    In the second argument of `($)', namely
      `fromSing (sing :: Sing (6 * 7))'
    In a stmt of a 'do' block: print $ fromSing (sing :: Sing (6 * 7))
-}