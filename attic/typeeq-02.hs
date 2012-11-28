{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Singletons

class TypeEq x y (b::Bool) | x y -> b
instance TypeEq x x True
instance b ~ False => TypeEq x y b

data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: h -> HList t -> HList (a ': t)

hxs :: HList [String, Integer]
hxs = HCons "answer" (HCons 42 HNil)


class Lookup a xs (ret :: Bool)
instance Lookup a (HList '[]) False
instance (TypeEq a b ret1,  Lookup a (HList t) ret2, (ret1 :|| ret2) ~ ret3)
                          => Lookup a (HList (b ': t)) ret3

main ::(Lookup String (HList '[String]) True) => IO ()
main = print "hi"
