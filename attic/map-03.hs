{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

import Data.Singletons
import Numeric.NumType

data TKVP :: [(*,*)] -> * where
  TKVPNil  :: TKVP '[]
  TKVPCons :: k -> v -> TKVP t -> TKVP ('(k,v) ': t)  

class Lookup k m v | k m -> v
 
instance Lookup k (TKVP ('(k, v) ': t) ) v
instance (Lookup k (TKVP b) v) =>  Lookup k (TKVP ('(k1,v1) ': b) ) v

data Meter = Meter
data Kilogram = Kilogram
data Second = Second

type Energy = TKVP '[ '(Meter, Pos2) ,  '(Kilogram, Pos1) ,  '(Second, Neg2)]

energy :: Energy
energy = TKVPCons Meter pos2 (TKVPCons Kilogram pos1 (TKVPCons Second neg2 TKVPNil))

toStr :: forall n1 n2 n3 m. (Lookup Meter m n1, Lookup Kilogram m n2, Lookup Second m n3, 
          Show n1, Show n2, Show n3) => m -> String
toStr _ = unwords xs
  where
    xs :: [String]
    xs = [show (undefined::n1), show (undefined::n2), show (undefined::n3)]

main = do
  print $ toStr energy

