{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
import Prelude hiding ((+),(*),(-),(/))
import UnitTyped
import UnitTyped.Currency
import UnitTyped.NoPrelude 
import UnitTyped.SI 
import UnitTyped.SI.Meta 
import UnitTyped.SI.Derived.Time 



main = do
  print $ 1 gram * 1 month + 8 hour * 10 gram


  print $ 1 gram * 1 month * bossSalary + workerSalary * 8 hour * 10 gram 

    where
      bossSalary   = 40e4 yen / 1 month / 1 gram
      workerSalary = 1000 yen / 1 hour / 1 gram

