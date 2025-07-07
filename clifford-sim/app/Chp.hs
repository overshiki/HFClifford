module Chp where 
import qualified Data.HashMap.Strict as HS

-- column: qubit index, row: stabilizer index
-- x11 ... x1n | z11 ... z1n | r1 
-- ... ... ... | ... ... ... | ...
-- xn1 ... xnn | zn1 ... znn | rn 
-- -------------------------------
-- x(n+1)1 ... x(n+1)n | z(n+1)1 ... z(n+1)n | r(n+1)
-- ...     ... ...     | ...     ... ...     | ...
-- x(2n)1  ... x(2n)n  | z(2n)1  ... z(2n)n  | r(2n)

class AppZip a where
  type Elem a  
  mapZip :: ((Elem a) -> (Elem a)) -> a -> a -> a 

instance AppZip ([a], [a]) where 
  type Elem ([a], [a]) = a 
  mapZip func (xa, ya) (xb, yb) = (xs, ys)
    where 
      xs = map (\(a, b) -> func a b) (zip xa xb)
      ys = map (\(a, b) -> func a b) (zip ya yb)

onesLike :: ([Bool], [Bool]) -> ([Bool], [Bool])
onesLike (xs, ys) = (take (length xs) $ repeat True, take (length ys) $ repeat True)

type QInd = Int 

type Table = HS.HashMap QInd ([Bool], [Bool])  


-- TODO: use data kind to have better constraint
data Tableau = Tableau
  { n :: Int
  , xtable :: Table
  , ztable :: Table
  , rvec    :: ([Bool], [Bool]) -- size 2n vector 
  , scratch :: ([Bool], [Bool]) -- size 2n vector as scratchpad
  }

-- -- swap x[:, a] with z[:, a]
-- swapxz :: QInd -> Tableau -> Tableau
-- swapxz qind tb@(Tableau {..}) = tb {xtable = _xtable, ztable = _ztable}
--   where 
--     xcolumn = fromJust $ HS.lookup qind xtable 
--     zcolumn = fromJust $ HS.lookup qind ztable 
--     _xtable = HS.insert qind zcolumn xtable 
--     _ztable = HS.insert qind xcolumn ztable

xor :: Bool -> Bool -> Bool 
xor True True = False 
xor True False = True 
xor False True = True 
xor False False = False

band :: Bool -> Bool -> Bool 
band True True = True 
band _ _ = False 

-- -- z[:, a] := z[:, a] `xor` x[:, a]
-- xorx2z :: QInd -> Tableau -> Tableau
-- xorx2z qind tb@(Tableau {..}) = tb {ztable = _ztable}
--   where 
--     xcolumn = fromJust $ HS.lookup qind xtable 
--     zcolumn = fromJust $ HS.lookup qind ztable 
--     _zcolumn = mapZip xor xcolumn zcolumn
--     _ztable = HS.insert qind _zcolumn ztable


-- swap x[:, a] with z[:, a]
-- r[:] := r[:] `xor` (x[:, a] `and` z[:, a])
hardmard :: QInd -> Tableau -> Tableau
hardmard qind tb@(Tableau {..}) = tb 
  { xtable = _xtable
  , ztable = _ztable
  , rvec = _rvec
  }
  where 
    xcolumn = fromJust $ HS.lookup qind xtable 
    zcolumn = fromJust $ HS.lookup qind ztable 
    _rvec = mapZip xor rvec (mapZip band xcolumn zcolumn)
    _xtable = HS.insert qind zcolumn xtable 
    _ztable = HS.insert qind xcolumn ztable

-- z[:, a] := z[:, a] `xor` x[:, a]
-- r[:] := r[:] `xor` (x[:, a] `and` z[:, a])
phase :: QInd -> Tableau -> Tableau
phase qind tb@(Tableau {..}) = tb 
  { ztable = _ztable
  , rvec = _rvec
  }
  where 
    xcolumn = fromJust $ HS.lookup qind xtable 
    zcolumn = fromJust $ HS.lookup qind ztable 
    _rvec = mapZip xor rvec (mapZip band xcolumn zcolumn)
    _zcolumn = mapZip xor xcolumn zcolumn
    _ztable = HS.insert qind _zcolumn ztable

-- z[:, a] := z[:, a] `xor` z[:, b]
-- x[:, b] := x[:, b] `xor` x[:, a]
-- r[:] := r[:] `xor` (x[:, a] `and` z[:, b] `and` (x[:, b] `xor` z[:, a] `xor` 1))
cnot :: QInd -> QInd -> Tableau -> Tableau
cnot qa qb tb@(Tableau {..}) = tb 
  { xtable = _xtable
  , ztable = _ztable
  , rvec = _rvec
  }
  where 
    xa = fromJust $ HS.lookup qa xtable 
    za = fromJust $ HS.lookup qa ztable 
    xb = fromJust $ HS.lookup qb xtable 
    zb = fromJust $ HS.lookup qb ztable 
    _za = mapZip xor za zb 
    _xb = mapZip xor xb xa 
    _rvec = mapZip xor rvec 
      $ mapZip and xa 
      $ mapZip and zb 
      $ mapZip xor xb 
      $ mapZip xor za (onesLike za)
    _ztable = HS.insert qa _za ztable
    _xtable = HS.insert qb _xb xtable