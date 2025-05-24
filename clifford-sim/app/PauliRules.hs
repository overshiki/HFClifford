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

data Sign = Pos | Neg
  deriving (Eq, Ord, Show)

data Pauli = X | Z | Y | I
  deriving (Eq, Ord, Show)

data SymExpr a = 
  a :*: a
  | Pauli :&: a
  | One Sign 
  | Img Sign
  | E
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

infix 6 :&:
infix 7 :*:

cost :: CostFunction SymExpr Int
cost = \case
  (a :*: b) -> a + b
  (One _) -> 1
  (Img _) -> 1
  E -> 0
  -- (_ :&: c) -> 1 + c
  -- c1 :&: c2 -> c1 + c2 + 5

-- -- XY = iZ 
-- -- YZ = iX 
-- -- ZX = iY
-- -- YX = -iZ 
-- -- ZY = -iX
-- -- XZ = -iY 
-- -- X^2 = Y^2 = Z^2 = I

rewrites :: [Rewrite () SymExpr]
rewrites =
  [ 
    -- for PivotNum: a * b = b * a
    pat (pat ("a" :*: "b") :*: "c") := pat (pat ("b" :*: "a") :*: "c")
    -- for PivotNum: (a * b) * c = a * (b * c)
    , pat (pat ("a" :*: "b") :*: "c") := pat ("a" :*: pat ("b" :*: "c"))
    -- for PivotNum: 1 * a = a
    , pat ((pat (One Pos)) :*: "a") := "a"
    -- for PivotNum: i * i = -1
    , pat (pat ((pat (Img Pos)) :*: (pat (Img Pos))) :*: "c") := pat ((pat (One Neg)) :*: "c")
    -- for PivotNum: (-1) * i = -i 
    , pat (pat ((pat (One Neg)) :*: (pat (Img Pos))) :*: "c") := pat ((pat (Img Neg)) :*: "c")
    -- for PivotNum: (-1) * (-1) = 1
    , pat (pat ((pat (One Neg)) :*: (pat (One Neg))) :*: "c") := pat ((pat (One Pos)) :*: "c")

  ]

rewrite :: Fix SymExpr -> Fix SymExpr
rewrite e = fst (equalitySaturation e rewrites cost)

(.*.) :: Fix SymExpr -> Fix SymExpr -> Fix SymExpr
a .*. b = Fix (a :*: b)

oneSign :: Sign -> Fix SymExpr
oneSign s = Fix (One s)

imgSign :: Sign -> Fix SymExpr
imgSign s = Fix (Img s)

-- (-1) * i * i
e1 :: Fix SymExpr
e1 = (oneSign Neg) .*. (imgSign Pos) .*. (imgSign Pos) .*. (Fix E)