{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module Main where 
import GHC.Generics (Generic(..))
-- import Foreign (Storable(..))
-- import Foreign.CStorable (CStorable(..))
import Foreign.C.Types
import Foreign.StablePtr
import Foreign.Ptr
import Foreign
import System.IO.Unsafe
import Foreign.Marshal.Array

import Ast
import Parse

-- foreign import ccall "bind_func"
--     bind_func :: CBool -> CInt -> CFloat -> IO ()

foreign import ccall "bind_test"
    bind_test :: CBool -> CInt -> CFloat -> IO ()

foreign import ccall "bind_test_vec"
    bind_test_vec :: CBool -> CInt -> CFloat -> Ptr CBool -> Ptr CInt -> CInt -> IO ()

-- subroutine prog(qubit_n, gate_n, prog_encoding, prn, measure_array, mn, pauli_res, pln)
foreign import ccall "prog"
    prog :: CInt -> CInt -> Ptr CInt -> CInt -> Ptr CBool -> CInt -> Ptr CBool -> CInt -> IO ()

data Prog = Prog 
  { nQubit :: CInt
  , nGate :: CInt
  , circuitEncoding :: [CInt]
  , encodingLength :: CInt
  , nMeasure :: CInt
  , measureResult :: [CBool]
  , pauli :: [CBool]
  , pauliLength :: CInt
  }

runProg :: Prog -> IO (Ptr CBool, Ptr CBool)
runProg (Prog { .. }) = do 
  cePtr <- newArray circuitEncoding
  mrPtr <- newArray measureResult
  pauliPtr <- newArray pauli
  prog nQubit nGate cePtr encodingLength mrPtr nMeasure pauliPtr pauliLength
  return (pauliPtr, mrPtr)

buildProg :: String -> IO Prog
buildProg file = do 
  s <- readFile "data/epr.chp"
  let 
    c@(Circuit gs) = runParser parseFile s 
    cencoding = map (CInt . fromIntegral) (encoding c)
    cenL = CInt $ fromIntegral $ length cencoding
    ng = CInt $ fromIntegral $ length gs
    nqubits = collectNumQubits c 
    nq = CInt $ fromIntegral $ nqubits
    nmeasure = collectMeasureNum c
    nm = CInt $ fromIntegral nmeasure
    measureResult = take nmeasure (repeat (CBool 0))
    pauliL = (2 * nqubits + 1) * nqubits
    pauliLength = CInt $ fromIntegral $ pauliL
    pauli = take pauliL (repeat (CBool 0))
  return $ Prog nq ng cencoding cenL nm measureResult pauli pauliLength

cbool2bool :: CBool -> Bool
cbool2bool cb = case toInteger cb of 
  0 -> False 
  1 -> True
  _ -> error "value error"

readout :: CInt -> Ptr CBool -> IO [Bool]
readout i ptr = map cbool2bool <$> peekArray ii ptr
  where 
    ii = fromIntegral i 

main :: IO ()
main = do
  let 
    b = CBool 0
    f = CFloat 3.14 
    i = CInt 10
    n = CInt 3
  -- print "in c and then fortran"
  -- bind_func b i f 
  print "in direct fortran"
  bind_test b i f

  p <- newArray [CBool 1, CBool 0, CBool 1]
  pi <- newArray [CInt 1, CInt 2, CInt 10]
  bind_test_vec b i f p pi n

  s <- readFile "data/epr.chp"
  let c = runParser parseFile s 
  print c

  prog@(Prog {..}) <- buildProg "data/epr.chp"
  (pauliPtr, mrPtr) <- runProg prog
  pauliRep <- readout pauliLength pauliPtr
  measures <- readout nMeasure mrPtr
  print pauliRep
  print measures
  return ()