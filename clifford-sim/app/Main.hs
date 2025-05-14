{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where 
import GHC.Generics (Generic(..))
-- import Foreign (Storable(..))
-- import Foreign.CStorable (CStorable(..))
import Foreign.C.Types
import Foreign.StablePtr
import Foreign.Ptr
import Foreign
import System.IO.Unsafe

foreign import ccall "call_plus"
    plus_test :: CBool -> IO ()

foreign import ccall "factorial" c_factorial :: CInt -> IO CInt

-- use c_factorial function from c Foreign Function
doFactorial :: Int -> IO Int
doFactorial n = do
  result <- c_factorial (fromIntegral n)
  return (fromIntegral result)

main :: IO ()
main = do
  let n = bit 0 :: CBool
  plus_test n 
  -- plus_test False
  dofac <- doFactorial 5
  print dofac
  return ()
    -- let 
    --     amplitude = 10
    --     carrierFreq = 3
    --     carrierPhase = 5
    --     dragAlpha = 8
    --     rSigma = 20
    
    -- print $ run amplitude carrierFreq carrierPhase dragAlpha rSigma
