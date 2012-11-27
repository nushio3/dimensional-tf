{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
import Prelude hiding ((+),(*),(-),(/))
import UnitTyped.NoPrelude 
import UnitTyped.SI 



alpha =  1 gram * 1 meter 
beta  =  1 gram * 1 meter * 1 second / 1 meter / 1 gram
gamma =  1 second

main = do
  print $ alpha
  print $ beta
  print $ gamma
  print $ gamma + beta
  print $ beta  + gamma
