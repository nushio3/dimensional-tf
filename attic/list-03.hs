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
type instance Lookup a (HList (a ': t)) = True
type instance Lookup a (HList (b ': t)) = Lookup a (HList t)


type IsThereInt1 = Lookup Int (HList '[])
type IsThereInt2 = Lookup Int (HList [String, Integer])

main = do
  print $ fromSing (sing :: Sing IsThereInt1)
  print $ fromSing (sing :: Sing IsThereInt2)

{-
$ runhaskell --version
runghc 7.6.1
$ runhaskell list-03.hs

list-03.hs:16:15:
    Conflicting family instance declarations:
      type instance Lookup a (HList ((':) * a t))
        -- Defined at list-03.hs:16:15
      type instance Lookup a (HList ((':) * b t))
        -- Defined at list-03.hs:17:15
-}
