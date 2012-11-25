{-# LANGUAGE DataKinds, TypeOperators #-}
import GHC.TypeLits

type Big1 = Sing ((2^99)^88)

main = do
  print $ fromSing (sing :: Big1)
  print $ map fromSing [sing :: Sing (2^400^10),sing :: Sing (2^10^400)]
