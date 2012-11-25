{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

import Data.Singletons

data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: h -> HList t -> HList (a ': t)  

hnull = HNil

hxs :: HList [String, Integer]
hxs = HCons "answer" (HCons 42 HNil)

type family Lookup a b :: Bool
type instance Lookup a (HList '[]) = False
type instance Lookup a (HList (b ': t)) = (a :==: b) :|| (Lookup a (HList t))


type IsThereInt1 = Lookup Int (HList '[])
type IsThereInt2 = Lookup Int (HList [String, Integer])

main = do
  print $ fromSing (sing :: Sing IsThereInt1)
  print $ fromSing (sing :: Sing IsThereInt2)

{-
$ runhaskell list-02.hs

list-02.hs:24:21:
    No instance for (SingI
                       Bool ((:==:) * Int [Char] :|| ((:==:) * Int Integer :|| 'False)))
      arising from a use of `sing'
    Possible fix:
      add an instance declaration for
      (SingI
         Bool ((:==:) * Int [Char] :|| ((:==:) * Int Integer :|| 'False)))
    In the first argument of `fromSing', namely
      `(sing :: Sing IsThereInt2)'
    In the second argument of `($)', namely
      `fromSing (sing :: Sing IsThereInt2)'
    In a stmt of a 'do' block:
      print $ fromSing (sing :: Sing IsThereInt2)
-}