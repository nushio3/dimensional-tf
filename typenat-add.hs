{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies,
KindSignatures, MultiParamTypeClasses, TypeOperators #-}

import GHC.TypeLits

type Six = (6 :: Nat)
type Seven = (7 :: Nat)
type FourtyTwo = (Six * Seven :: Nat)

main = do
  print $ (sing :: Sing 42)
  print $ (sing :: Sing FourtyTwo)
