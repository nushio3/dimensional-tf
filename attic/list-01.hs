{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving, TypeFamilies, TypeOperators #-}

data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: h -> HList t -> HList (a ': t)  


hxs :: HList [String, Integer]
hxs = HCons "answer" (HCons 42 HNil)

type family Lookup a b :: Bool
type instance Lookup a (HList '[]) = False
type instance Lookup a (HList (a ': t)) = True
type instance Lookup a (HList (b ': t)) = Lookup a (HList t)


main = print "hi"

