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
  | Flow String [QIndex]
  deriving (Show)

data Pauli = X | Z | Y | I
  deriving (Eq, Ord, Show)

data FlowGate = FlowGate [(Pauli, Pauli)]
  deriving (Eq, Ord, Show)

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

class BEncoding a where
  bencoding :: a -> [Bool]

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

newtype SingleGLookUp = SingleGLookUp [(Bool, Bool, Bool)]

instance BEncoding SingleGLookUp where
  bencoding (SingleGLookUp bs) = bpack bs
    where
      bpack :: [(Bool, Bool, Bool)] -> [Bool]
      bpack ((b1, b2, b3):rbs) = b1:b2:b3:(bpack rbs)
      bpack [] = []

newtype SingleStabFlow = SingleStabFlow (Bool -> Bool -> Bool -> (Bool, Bool, Bool))

hFlow :: SingleStabFlow
hFlow = SingleStabFlow func
  where
    func xia zia ri = (zia, xia, ri `xor` (xia && zia))

type BitWidth = Int
int2boolL_ :: [Bool] -> BitWidth -> Int -> [Bool]
int2boolL_ blist 0 0 = blist
int2boolL_ _ 0 _ = error "value error: input integer is larger than 2^bitwidth"
int2boolL_ blist bw b = int2boolL_ (bt:blist) (bw - 1) nb
  where
    bt = odd b
    nb = b `div` 2

int2boolL :: BitWidth -> Int -> [Bool]
int2boolL = int2boolL_ []
 

singleFlow2lookup :: SingleStabFlow -> SingleGLookUp
singleFlow2lookup (SingleStabFlow func) = SingleGLookUp
  $ take 8 $ map (nfunc . int2boolL 3) [0 .. ]
  where 
    nfunc [x, y, z] = func x y z
    nfunc _ = error "value error"


hLookUp :: SingleGLookUp
hLookUp = singleFlow2lookup hFlow

pFlow :: SingleStabFlow
pFlow = SingleStabFlow func
  where
    func xia zia ri = (xia, xia `xor` zia, ri `xor` (xia && zia))

pLookUp :: SingleGLookUp
pLookUp = singleFlow2lookup pFlow

-- 1 for hardmard 
-- 2 for phase
gateRepresentation :: [Bool]
gateRepresentation = (bencoding hLookUp) ++ (bencoding pLookUp)