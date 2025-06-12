{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module Main where 
import GHC.Generics (Generic(..))
import Foreign.C.Types
import Foreign.StablePtr
import Foreign.Ptr
import Foreign
import System.IO.Unsafe
import Foreign.Marshal.Array
import Data.List
import System.Environment (getArgs)
import qualified Data.HashMap.Strict as HS
import System.IO

import Ast
import Parse
import PauliRules
import Infer
import Encoding

foreign import ccall "prog"
    prog :: CInt -> CInt           -- qubit_num -> gate_num 
            -> Ptr CInt -> CInt    -- prog_encoding -> length
            -> Ptr CBool -> CInt   -- measure_array -> length
            -> Ptr CBool -> CInt   -- pauli_res -> length 
            -> Ptr CBool -> CInt   -- gate_rep
            -> Ptr CBool           -- measure_rand
            -> IO ()

int2cint :: Int -> CInt
int2cint = CInt . fromIntegral

data VecPac a = VecPac
  { d :: [a]
  , dl :: CInt
  }

fromVec :: [a] -> VecPac a 
fromVec xs = VecPac xs (int2cint $ length xs)

data Prog = Prog 
  { nQubit :: CInt
  , nGate :: CInt
  , circEncode :: VecPac CInt
  , measureRes :: VecPac CBool
  , pauli :: VecPac CBool
  , gateRep :: VecPac CBool
  , measureRand :: VecPac CBool
  }

runProg :: Prog -> IO (Ptr CBool, Ptr CBool, Ptr CBool)
runProg (Prog { .. }) = do 
  cePtr <- newArray (d circEncode)
  mrPtr <- newArray (d measureRes)
  mrandPtr <- newArray (d measureRand)
  pauliPtr <- newArray (d pauli)
  gateRepPtr <- newArray (d gateRep)
  prog nQubit nGate 
    cePtr (dl circEncode) 
    mrPtr (dl measureRes) 
    pauliPtr (dl pauli)
    gateRepPtr (dl gateRep)
    mrandPtr
  return (pauliPtr, mrPtr, mrandPtr)

buildProg :: String -> IO Prog
buildProg file = do 
  s <- readFile file
  let 
    (env, c@(Circuit gs)) = runParser parseFile s 
    nq = collectNumQubits c 
    ng = length gs
    nm = collectMeasureNum c
    pauliL = (2 * nq + 1) * nq
    -- flows = map flowDef2lookup (HS.elems env)
  -- print (HS.keys env)
  -- print (flows !! 0 )
  return $ Prog
    { nQubit = int2cint nq
    , nGate = int2cint ng
    , circEncode = fromVec $ map int2cint (encoding (env, c))
    , measureRes = fromVec $ take nm (repeat (CBool 0))
    , pauli = fromVec $ take pauliL (repeat (CBool 0))
    , gateRep = fromVec (map bool2cbool (gateRepresentation env))
    , measureRand = fromVec $ take nm (repeat (CBool 0))
    }

cbool2bool :: CBool -> Bool
cbool2bool cb = case toInteger cb of 
  0 -> False 
  1 -> True
  _ -> error "value error"

bool2cbool :: Bool -> CBool
bool2cbool False = CBool 0 
bool2cbool True = CBool 1

readout :: CInt -> Ptr CBool -> IO [Bool]
readout i ptr = map cbool2bool <$> peekArray ii ptr
  where 
    ii = fromIntegral i 

getPauliRepEach :: Int -> [Bool] -> String
getPauliRepEach n (b:bs) = brep ++ bsrep 
  where 
    -- r is 1 if r has negative phase
    -- r is 0 if r has postive phase
    brep = if b then "-" else "+"
    decodeFunc :: (Bool, Bool) -> String
    decodeFunc (x, z) = case (x, z) of 
      (True,  False) -> "X"
      (False, True)  -> "Z"
      (True,  True)  -> "Y"
      (False, False) -> "I"
    decodeSeq :: [Bool] -> String
    decodeSeq (x:z:remains) = decodeFunc (x, z) ++ decodeSeq remains
    decodeSeq [] = ""

    bsrep = decodeSeq bs

getPauliRep :: Int -> [Bool] -> String 
getPauliRep n bs@(b:_) = (getPauliRepEach n cs) ++ "\n" ++ (getPauliRep n remains)
  where 
    eachLength = 2 * n + 1
    (cs, remains) = if (length bs) >= eachLength 
      then splitAt eachLength bs 
      else error "value error"
getPauliRep n [] = ""


logFresh :: Maybe String -> IO ()
logFresh Nothing = return ()
logFresh (Just file) = writeFile file ""

logAppend :: Maybe String -> String -> IO ()
logAppend Nothing s = putStrLn s 
logAppend (Just file) s = appendFile file (s ++ "\n")

run :: Maybe String -> String -> IO ()
run mlog file = do 
  prog@(Prog {..}) <- buildProg file
  (pauliPtr, mrPtr, mrandPtr) <- runProg prog
  pauliRep <- readout (dl pauli) pauliPtr
  measures <- readout (dl measureRes) mrPtr
  measureIsRandom <- readout (dl measureRand) mrandPtr
  logFresh mlog 
  let logout = logAppend mlog
  logout $ "program to run: " ++ file
  logout $ "nQubit: " ++ show nQubit
  -- logout $ "pauli Length: " ++ show (dl pauli)
  logout $ "pauliRep:"
  logout $ getPauliRep (fromIntegral nQubit) pauliRep
  -- print pauliRep
  logout $ "measureRes:"
  logout $ show measures
  logout $ "measure is random:"
  logout $ show measureIsRandom
  logout "\n"
  return ()

test :: IO ()
test = do
  -- putStrLn "welcome to HFClifford!"
  -- let runlog = run Nothing
  -- run (Just "data/epr.out") "data/epr.chp"
  -- run (Just "data/ghz.out") "data/ghz.chp"
  -- run (Just "data/teleport.out") "data/teleport.chp"
  -- run (Just "data/simon.out") "data/simon.chp"
  run (Just "data/densecoding.out") "data/densecoding.chp"
  -- run (Just "data/qecc9.out") "data/qecc9.chp"

main :: IO ()
main = do
  test
  [file] <- getArgs
  putStrLn "welcome to HFClifford!"
  run Nothing file

  -- let exprs = map rewrite pauliRuleTests
  -- mapM_ print exprs