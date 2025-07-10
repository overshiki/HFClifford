module Chp where 
import qualified Data.HashMap.Strict as HS
import Data.List

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
  (!!!) :: a -> Int -> Elem a
  update :: Int -> Elem a -> a -> a

-- assert: i <= length xs - 1
updateAt :: Int -> a -> [a] -> [a]
updateAt i x xs = ps ++ (x:remain)
  where 
    -- this is safe, since i <= length xs - 1
    (ps, _:remain) = splitAt i xs 

instance AppZip ([a], [a]) where 
  type Elem ([a], [a]) = a 
  mapZip func (xa, ya) (xb, yb) = (xs, ys)
    where 
      xs = map (\(a, b) -> func a b) (zip xa xb)
      ys = map (\(a, b) -> func a b) (zip ya yb)
  (!!!) (xs, ys) i = if i < l 
    then xs !! i 
    else ys !! (i - l) 
    where 
      l = length xs 
  update i e (xs, ys) = if i < l 
    then (updateAt i e xs, ys)
    else (xs, updateAt (i - l) e ys)
    where 
      l = length xs

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

-- check if there exists a p in {n+1, 2n} such that x[p, a] = 1
getRandomPivot :: QInd -> Tableau -> Maybe Ind 
getRandomPivot qind tb@(Tableau {..}) = idx
  where
    -- x corresponding to {n+1, 2n}
    (_, x) = fromJust $ HS.lookup qind xtable 
    idx = (n + 1 + ) <$> findIndex id x

-- Z: 0, P: 1, N: -1, T: 2, Th: 3, Nt: -2, Nth: -3
data Exponent = Z | P | N 
  | T | Th 
  | Nt | Nth

fromInt :: Int -> Exponent 
fromInt 0 = Z 
fromInt 1 = P 
fromInt 2 = T 
fromInt 3 = Th 
fromInt -1 = N 
fromInt -2 = Nt 
fromInt -3 = Nth
fromInt _ = error "value error"

fromBool :: Bool -> Exponent 
fromBool True = P 
fromBool False = Z 

double :: Exponent -> Exponent
double x = addmod x x  

suc :: Exponent -> Exponent 
suc Nth = Nt  
suc Nt = N 
suc N = Z 
suc Z = P 
suc P = T 
suc T = Th 
suc Th = Z

pre :: Exponent -> Exponent 
pre Th = T 
pre T = P 
pre P = Z 
pre Z = N 
pre N = Nt 
pre Nt = Nth 
pre Nth = Z

-- (a + b) mod 4
addmod :: Exponent -> Exponent -> Exponent 
addmod Z x = x 
addmod P x = suc x 
addmod N x = pre x 
addmod T x = suc $ suc x 
addmod Th x = suc $ suc $ suc x 
addmod Nt x = pre $ pre x 
addmod Nth x = pre $ pre $ pre x

-- x1 z1 x2 z2
gFunc :: Bool -> Bool -> Bool -> Bool -> Exponent 
gFunc False False _ _ = Z 
gFunc True True x2 z2 = case (z2, x2) of 
  -- z2 - x2
  (True, True) -> Z 
  (True, False) -> P
  (False, True) -> N 
  (False, False) -> Z 
gFunc True False x2 z2 = case (z2, x2) of  
  -- z2(2x2 - 1)
  (False, _) -> Z 
  (True, False) -> N 
  (True, True) -> P
gFunc False True x2 z2 = case (z2, x2) of 
  -- x2(1 - 2z2)
  (_, False) -> Z 
  (True, True) -> N 
  (False, True) -> P

-- rowsum :: Ind -> Ind -> Tableau -> Tableau
rowsum h i tb@(Tableau {..}) = tb 
  { xtable = _xtable
  , ztable = _ztable
  , rvec = _rvec
  }
  where 
    qs = [0 .. n - 1]
    v = foldl1 addmod
      $ map (\qj -> let
        x = fromJust $ HS.lookup qj xtable 
        z = fromJust $ HS.lookup qj ztable 
        xij = x !!! i 
        zij = z !!! i 
        xhj = x !!! h 
        zhj = z !!! h
      in gFunc xij zij xhj zhj
      ) qs
    rh = fromBool $ rvec !!! h 
    ri = fromBool $ rvec !!! i
    res = (double rh) `addmod` (double ri) `addmod` v
    _rvec = case res of 
      -- 0 mod 4
      Z -> updateAt h False rvec
      -- 2 mod 4
      T -> updateAt h True rvec
      _ -> error "value error"

    tableFunc t = map 
      (\qj -> let  
          v = fromJust $ HS.lookup qj t
          vij = v !!! i 
          vhj = v !!! h 
          nv = updateAt h (vij `xor` vhj) v
        )
      qs

    _xtable = tableFunc xtable
    _ztable = tableFunc ztable

-- measure :: 