{-# LANGUAGE DeriveFunctor, TypeFamilies, EmptyDataDecls, PolyKinds, DataKinds, ConstraintKinds #-}

newtype Dimensional (dim :: Dimension) a = Dimensional a
  deriving (Eq, Ord, Show, Functor)

data Dimension = Dim0 | Dim String Int Dimension

type family Insert (a :: Dimension) (k :: String) (v :: Int) :: Dimension
type instance Insert (Dim0 k v) = Dim k v Dim0
type instance (k < a) => Insert (Dim a b l) k v = Dim k v (Dim a b l)
type instance Insert (Dim a b l) k v = Dim a b (Insert l k v)

type family Sort' (a :: Dimension) (b :: Dimension) :: Dimension
type instance Sort' ls Dim0 = ls
type instance Sort' ls (Dimension k v rs) =
  Sort' (Insert ls k v) rs

type family Sort (a :: Dimension) :: Dimension
type instance Sort ls = Sort' ls Dim0

type family Merge (a :: Dimension) (b :: Dimension) :: Dimension
type family

type family Mult (a :: Dimension) (b :: Dimension) :: Dimension
type family Mult a b = Merge (Sort a) (Sort b)

(<+>) :: Num a => Dimensional dim a -> Dimensional dim a -> Dimensional dim a
Dimensional a <+> Dimensional b = Dimensional (a + b)

(<->) :: Num a => Dimensional dim a -> Dimensional dim a -> Dimensional dim a
Dimensional a <-> Dimensional b = Dimensional (a - b)

(<*>) :: Num a
         => Dimensional (dim1 :: Dimension) a
         -> Dimensional (dim2 :: Dimension) a
         -> Dimensional (Mult dim1 dim2) a
Dimensional a <*> Dimensional b = Dimensional (a * b)

main :: IO ()
main = do
  let dist = Dimensional 123 :: Dimensional (Dim "meter" 1 Dim0) Double
      time = Dimensional 60 :: Dimensional (Dim "second" 1 Dim0) Double
  print $ dist <*> time