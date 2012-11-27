{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
import Prelude hiding ((+),(*),(-),(/))
import UnitTyped.NoPrelude 
import UnitTyped.SI 



alpha =  1 gram * 1 meter 
beta  =  1 gram * 1 meter * 1 second / 1 meter / 1 gram
gamma =  1 second

main = do
  print $ alpha         -- 1.0 g⋅m      
  print $ beta          -- 1.0 g⋅m⋅s/m/g
  print $ gamma         -- 1.0 s        
  print $ gamma + beta  -- 2.0 s        
  print $ beta  + gamma -- 2.0 g⋅m⋅s/m/g






