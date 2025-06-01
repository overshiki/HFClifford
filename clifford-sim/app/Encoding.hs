{-# LANGUAGE FlexibleInstances #-}
module Encoding where 
import Ast 
import Infer 
import qualified Data.HashMap.Strict as HS
import Data.Maybe
import Data.List

class Encoding a where
  encoding :: a -> [Int]

instance Encoding Gate where
  -- -1 for cnot 
  encoding (CNOT (QIndex i) (QIndex j)) = [-1, i + 1, j + 1]
  -- 0 for measure 
  encoding (M (QIndex i)) = [0, i + 1, 0]
  -- 1 for hardmard
  encoding (H (QIndex i)) = [1, i + 1, 0]
  -- 2 for phase
  encoding (P (QIndex i)) = [2, i + 1, 0]
  -- 3 .. for flow defined in FlowDef 
  encoding (Flow name (QIndex i)) = error "error in encoding flow"
  encoding c = error ("encoding fail for gate: " ++ show c)

instance Encoding [Gate] where
  encoding gs = concatMap encoding gs

instance Encoding Circuit where
  encoding (Circuit gs) = encoding gs

instance Encoding (Env, Gate) where 
  encoding (env, g) = case g of 
    (H {}) -> encoding g 
    (P {}) -> encoding g 
    (CNOT {}) -> encoding g 
    (M {}) -> encoding g 
    (Flow name (QIndex i)) -> 
      let 
        gid = fromJust $ HS.lookup name $ keyIds env
      in [gid, i + 1, 0]
    _ -> error ("encoding fail for gate: " ++ show g)

instance Encoding (Env, [Gate]) where
  encoding (env, gs) = concatMap (\g -> encoding (env, g)) gs

instance Encoding (Env, Circuit) where
  encoding (env, (Circuit gs)) = encoding (env, gs)

sortedKeys :: Env -> [String]
sortedKeys env = sort $ HS.keys env

keyIds :: Env -> HS.HashMap String Int
keyIds env = HS.fromList $ zip keys [3 .. ] -- start from 3  
  where 
    keys = sortedKeys env

hFlow :: SingleStabFlow
hFlow = SingleStabFlow func
  where
    func xia zia ri = (zia, xia, ri `xor` (xia && zia))

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
-- 3 .. for flows defined by FlowDef
gateRepresentation :: Env -> [Bool]
gateRepresentation env = (bencoding hLookUp) ++ (bencoding pLookUp) ++ (concat ens)
  where 
    ks = sortedKeys env
    ens = map (
      \k -> 
        let
          fd = fromJust $ HS.lookup k env
          lk = flowDef2lookup fd 
        in bencoding lk
      ) ks