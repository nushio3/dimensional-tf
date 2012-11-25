{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

import Data.Singletons
import Numeric.NumType

data TKVP :: [(*,*)] -> * where
  TKVPNil  :: TKVP '[]
  TKVPCons :: k -> v -> TKVP t -> TKVP ('(k,v) ': t)  

class Lookup k m v | k m -> v
 
instance Lookup k (TKVP ('(k, v) ': t) ) v
instance (Lookup k (TKVP b) v) =>  Lookup k (TKVP ('(k1,v1) ': b) ) v

data Meter
data Kilogram 
data Second

type Energy = TKVPCons Int Int TKVPNil -- TKVPCons Meter Pos2 (TKVPCons Kilogram Pos1 (TKVPCons Second Neg1 TKVPNil))

main = do
  print 1

