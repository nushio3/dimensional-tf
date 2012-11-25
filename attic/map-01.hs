{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}

import Data.Singletons

data TKVP :: [(*,*)] -> * where
  TKVPNil  :: TKVP '[]
  TKVPCons :: k -> v -> TKVP t -> TKVP ('(k,v) ': t)  

class Lookup k m v | k m -> v
 
instance Lookup k (TKVP ('(k, v) ': t) ) v
instance (Lookup k (TKVP b) v) =>  Lookup k (TKVP ('(k1,v1) ': b) ) v


main = do
  print 1

