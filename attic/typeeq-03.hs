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

hxs :: HList '[String, Integer]
hxs = HCons "answer" (HCons 42 HNil)

class IsNull a (ret :: Bool) | a -> ret
instance IsNull (HList '[]) True
instance IsNull (HList (a ': b)) False

class Lookup a xs (ret :: Bool) | a xs -> ret
instance Lookup a (HList '[]) False
instance (TypeEq a x ret1,
                   Lookup a (HList xs) ret2, (ret1 :|| ret2) ~ ret3)
                          => Lookup a (HList (x ': xs)) ret3

main ::(Lookup Int (HList '[String,Int]) True) => IO ()
main = main2

main2 ::(Lookup Integer (HList '[String,Int]) False) => IO ()
main2 = print "hi"
