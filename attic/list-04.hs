{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

import Data.Singletons

data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: h -> HList t -> HList (a ': t)  

hnull = HNil

hxs :: HList [String, Integer]
hxs = HCons "answer" (HCons 42 HNil)

class Lookup a b

instance Lookup a (HList (a ': t) )
instance (Lookup a (HList b)) =>  Lookup a (HList (c ': b) )


main = do
  print 1

