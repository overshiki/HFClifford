{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Ast where 
import Data.Hashable
import GHC.Generics

-- Qubit index, as column in CHP paper 
newtype QIndex = QIndex Int
  deriving (Show, Eq, Generic, Hashable)

-- stabilizer index, as row in CHP paper
newtype SIndex = SIndex Int
  deriving (Show, Eq, Generic, Hashable)

data Gate 
  = H QIndex
  | CNOT QIndex QIndex
  | M QIndex
  | P QIndex 
  deriving (Show)

newtype Circuit = Circuit [Gate]
  deriving (Show)

collectNumQubits_ :: Int -> Circuit -> Int
collectNumQubits_ n (Circuit (g:gs)) = collectNumQubits_ (max mindex n) (Circuit gs)
  where 
    mindex = case g of 
      (H (QIndex i)) -> i 
      (CNOT (QIndex i) (QIndex j)) -> max i j 
      (M (QIndex i)) -> i 
      (P (QIndex i)) -> i
collectNumQubits_ n (Circuit []) = n 

collectNumQubits :: Circuit -> Int 
collectNumQubits c = collectNumQubits_ 0 c + 1 -- it starts from 0, in future, use more robust range treatment

collectMeasureNum_ :: Int -> Circuit -> Int
collectMeasureNum_ n (Circuit (g:gs)) = collectMeasureNum_ nn (Circuit gs)
  where 
    nn = case g of 
      (M _) -> n + 1
      _ -> n 
collectMeasureNum_ n (Circuit []) = n 

collectMeasureNum :: Circuit -> Int 
collectMeasureNum c = collectMeasureNum_ 0 c

class Encoding a where 
  encoding :: a -> [Int]

instance Encoding Gate where 
  -- 1 for hardmard
  encoding (H (QIndex i)) = [1, i + 1, 0]
  -- 2 for phase
  encoding (P (QIndex i)) = [2, i + 1, 0]
  -- 3 for cnot 
  encoding (CNOT (QIndex i) (QIndex j)) = [3, i + 1, j + 1]
  -- 4 for measure 
  encoding (M (QIndex i)) = [4, i + 1, 0]

instance Encoding [Gate] where 
  encoding gs = concatMap encoding gs

instance Encoding Circuit where 
  encoding (Circuit gs) = encoding gs


