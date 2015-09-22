{-#LANGUAGE DeriveDataTypeable #-}
module Main where

{-
 This program generates an Integer -> Integer lookup table for
 the CAL programming language. This might be useful if computing
 the lookup function on an FPGA is too intensive.

 To use, define the lower and upper bounds of the lookup and also
 define the @lookup@ function.
-}

import Control.Monad (void)
import System.Console.CmdArgs
import CalLookupGen

import Data.Binary.Put
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy.Char8 as S
import Data.ByteString.Builder
import Numeric

lower, upper :: Int
lower = 0
upper = 65535

lookupFun :: Int -> String
lookupFun i =
    let x = runPut $ putFloat32be (fromIntegral i)
        bs = toLazyByteString (lazyByteStringHex x)
        myInt = read ("0x" ++ S.unpack bs)
    in "0x" ++ showHex myInt ""

inputType, outputType :: String
inputType  = "uint(size=16)"
outputType = "uint(size=32)"

main :: IO ()
main = do
  void getOpts -- for --help option
  mapM_ putStrLn (genFunArr lower upper inputType outputType lookupFun)

---------------------
-- command line options

data CalLookupOptions = CalLookupOptions
    { } deriving (Data, Typeable, Show, Eq)

calLookupOpts :: CalLookupOptions
calLookupOpts = CalLookupOptions
    { }

getOpts :: IO CalLookupOptions
getOpts = cmdArgs $ calLookupOpts
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME, _PROGRAM_INFO, _PROGRAM_ABOUT, _COPYRIGHT :: String
_PROGRAM_NAME = "cal-lookuptable-gen"
_PROGRAM_INFO = _PROGRAM_NAME
_PROGRAM_ABOUT = "Generates CAL implementation of a Haskell defined lookup function"
_COPYRIGHT = "(C) Rob Stewart, Rathlin Project 2015"
