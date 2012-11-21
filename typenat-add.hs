{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies,
KindSignatures, MultiParamTypeClasses, TypeOperators #-}

import GHC.TypeLits

data Index (i :: Nat) = Get

class Has a index b | a index -> b where
  from :: a -> Index index -> b

data Person = Person String String Int Int deriving Show

instance Has Person 0 String where from (Person x _ _ _) _ = x
instance Has Person 1 String where from (Person _ x _ _) _ = x
instance Has Person 2 Int    where from (Person _ _ x _) _ = x
instance Has Person 3 Int    where from (Person _ _ _ x) _ = x

einstein = Person "Albert" "Einstein" 1879 1955

main = do
  print $ from einstein (Get :: Index 0)
  print $ from einstein (Get :: Index 1)
  print $ from einstein (Get :: Index (1+1))
