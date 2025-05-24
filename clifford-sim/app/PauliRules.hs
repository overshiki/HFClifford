{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module PauliRules where
import Data.Equality.Utils
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Analysis
import Data.Equality.Graph
import Data.Equality.Graph.Lens

data IdNum = One | Img
  deriving (Eq, Ord, Show)

data PivotNum = Pos IdNum 
  | Neg IdNum
  deriving (Eq, Ord, Show)

data Pauli = X | Z | Y | I
  deriving (Eq, Ord, Show)

data SymExpr a = 
  a :*: a
  | a :&: a
  | P Pauli
  | N PivotNum
  | E
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

infix 6 :&:
infix 7 :*:

cost :: CostFunction SymExpr Int
cost = \case
  (a :*: b) -> a + b
  (a :&: b) -> a + b + 2
  (P _) -> 1
  (N _) -> 1
  E -> 0

rewrites :: [Rewrite () SymExpr]
rewrites =
  [ 
    -- for PivotNum: a * b = b * a
    pat (pat ("a" :*: "b") :*: "c") := pat (pat ("b" :*: "a") :*: "c")
    -- for PivotNum: (a * b) * c = a * (b * c)
    , pat (pat ("a" :*: "b") :*: "c") := pat ("a" :*: pat ("b" :*: "c"))
    -- for PivotNum: 1 * a = a
    , pat ((pat (N (Pos One))) :*: "a") := "a"
    -- -- for PivotNum: i * i = -1
    , pivotFunc (Pos Img) (Pos Img) (Neg One)
    -- -- for PivotNum: (-1) * i = -i 
    , pivotFunc (Neg One) (Pos Img) (Neg Img)
    -- -- for PivotNum: (-1) * (-1) = 1
    , pivotFunc (Neg One) (Neg One) (Pos One)

    -- for pauli: (a & b) & c = a & (b & c)
    , pat (pat ("a" :&: "b") :&: "c") := pat ("a" :&: pat ("b" :&: "c"))
    -- XY = iZ 
    , pauliFunc X Y (Pos Img) Z
    -- YZ = iX 
    , pauliFunc Y Z (Pos Img) X 
    -- ZX = iY
    , pauliFunc Z X (Pos Img) Y 
    -- YX = -iZ 
    , pauliFunc Y X (Neg Img) Z 
    -- ZY = -iX
    , pauliFunc Z Y (Neg Img) X 
    -- XZ = -iY 
    , pauliFunc X Z (Neg Img) Y 
    -- -- X^2 = Y^2 = Z^2 = I
    , pauliFunc X X (Pos One) I
    , pauliFunc Y Y (Pos One) I
    , pauliFunc Z Z (Pos One) I
  ]
  where 
    pivotFunc :: PivotNum -> PivotNum -> PivotNum -> Rewrite () SymExpr
    pivotFunc n1 n2 n3 = pat (pat ((pat (N n1)) :*: (pat (N n2))) :*: "c") := pat ((pat (N n3)) :*: "c")

    pauliFunc :: Pauli -> Pauli -> PivotNum -> Pauli -> Rewrite () SymExpr
    pauliFunc p1 p2 piv p3 = pat ((pat (P p1)) :&: (pat (P p2))) := pat (pat (N piv) :*: (pat (P p3)))


rewrite :: Fix SymExpr -> Fix SymExpr
rewrite e = fst (equalitySaturation e rewrites cost)

(.*.) :: Fix SymExpr -> Fix SymExpr -> Fix SymExpr
a .*. b = Fix (a :*: b)

(.&.) :: Fix SymExpr -> Fix SymExpr -> Fix SymExpr
a .&. b = Fix (a :&: b)

pivot :: PivotNum -> Fix SymExpr
pivot n = Fix (N n)

pauli :: Pauli -> Fix SymExpr 
pauli p = Fix (P p)

-- (-1) * i * i
e1 :: Fix SymExpr
e1 = (pivot (Neg One)) .*. (pivot (Pos Img)) .*. (pivot (Pos Img)) .*. (((pauli X) .&. (pauli Y)) .&. (Fix E))