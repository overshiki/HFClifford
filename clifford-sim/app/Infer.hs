module Infer where 
import Data.Equality.Utils
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Analysis
import Data.Equality.Graph
import Data.Equality.Graph.Lens

import Ast 
import PauliRules 

import qualified Data.HashMap.Strict as HS
import Data.Maybe
import System.IO.Unsafe

type FlowMap = HS.HashMap Pauli Pauli

flowDef2flowMap :: FlowDef -> FlowMap
flowDef2flowMap (FlowDef ps) = HS.fromList ps

-- currently we assume FlowDef must has X and Z as its input. Support more general cases in the future
conjugate :: FlowDef -> Pauli -> Fix SymExpr
conjugate fd p = case p of 
  X -> pauliMapFunc fFunc (pauli X)
  Z -> pauliMapFunc fFunc (pauli Z) 
  I -> pauli I 
  Y -> rewrite $ pauliMapFunc fFunc y 
  where 
    flow = flowDef2flowMap fd 
    fFunc :: Pauli -> Pauli 
    fFunc X = fromJust $ HS.lookup X flow
    fFunc Z = fromJust $ HS.lookup Z flow
    fFunc _ = error "should not be called here"

-- Fix SymExpr into tabel representation: (x, z, r)
unpackSymExpr :: Fix SymExpr -> (Bool, Bool, Bool)
unpackSymExpr (Fix (PL X)) = (True, False, False)
unpackSymExpr (Fix (PL Z)) = (False, True, False)
unpackSymExpr (Fix (PL Y)) = (True, True, False)
unpackSymExpr (Fix (PL I)) = (False, False, False)
unpackSymExpr (Fix ((Fix (N n)) :*: a)) = case n of 
  (Pos One) -> (x, y, False)
  (Neg One) -> (x, y, True)
  other -> error ("value error, unexpected: " ++ show other ++ show a)
  where 
    (x, y, r) = unpackSymExpr a 
unpackSymExpr (Fix (a :*: (Fix (N n)))) = case n of
  (Pos One) -> (x, y, False)
  (Neg One) -> (x, y, True)
  _ -> error "value error"
  where 
    (x, y, r) = unpackSymExpr a 
unpackSymExpr (Fix (a :&: (Fix E))) = unpackSymExpr a
unpackSymExpr o = error ("value error: " ++ show o)

flowDef2SingleStabFlow :: FlowDef -> SingleStabFlow
flowDef2SingleStabFlow fd = SingleStabFlow func
  where 
    -- xc = unpackSymExpr $ unsafePerformIO $ do {print "x"; print $ conjugate fd X; return $ conjugate fd X}
    -- zc = unpackSymExpr $ unsafePerformIO $ do {print "z"; print $ conjugate fd Z; return $ conjugate fd Z}
    -- yc = unpackSymExpr $ unsafePerformIO $ do {print "y"; print $ conjugate fd Y; return $ conjugate fd Y}
    -- ic = unpackSymExpr $ unsafePerformIO $ do {print "i"; print $ conjugate fd I; return $ conjugate fd I}
    xc = unpackSymExpr $ conjugate fd X
    zc = unpackSymExpr $ conjugate fd Z
    yc = unpackSymExpr $ conjugate fd Y 
    ic = unpackSymExpr $ conjugate fd I
    notR (x, y, r) = (x, y, not r)
    func :: Bool -> Bool -> Bool -> (Bool, Bool, Bool)
    func True False False = xc 
    func False True False = zc 
    func False False False = ic 
    func True True False = yc 
    func a b True = notR (func a b False)

flowDef2lookup :: FlowDef -> SingleGLookUp
flowDef2lookup  = singleFlow2lookup . flowDef2SingleStabFlow