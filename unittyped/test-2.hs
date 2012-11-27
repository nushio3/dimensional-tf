{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
import Prelude hiding ((+),(*),(-),(/))
import UnitTyped
import UnitTyped.Currency
import UnitTyped.NoPrelude 
import UnitTyped.SI 
import UnitTyped.SI.Meta 
import UnitTyped.SI.Derived.Time 


data HumanResource
type HumanResourceUnit = UnitCons HumanResource (Pos One) UnitNil

data Man

instance Convertable HumanResourceUnit Man where
  factor _ = 1
  showunit _ _ = "man"         


man :: (Fractional f) => Value f HumanResourceUnit Man
man = one

main = do
  print $ 1 man * 1 month + 8 hour * 10 man
  print $ 1 man * 1 month * bossSalary + workerSalary * 8 hour * 10 man 
    where
      bossSalary   = 40e4 yen / 1 month / 1 man
      workerSalary = 1000 yen / 1 hour / 1 man

