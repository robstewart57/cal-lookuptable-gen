{-#LANGUAGE DeriveDataTypeable #-}
module Main where

{-
 This program generates an Integer -> Integer lookup table for
 the CAL programming language. This might be useful if computing
 the lookup function on an FPGA is too intensive.

 To use, define the lower and upper bounds of the lookup and also
 define the lookup functin. Comment out in `main` whether you want
 a procedure or a function CAL implementation of the lookup table.
-}

import Control.Monad (void)
import System.Console.CmdArgs
import CalLookupGen

lower, upper :: Int
lower = 1
upper = 20

lookupFun :: Int -> String
lookupFun = show . round .  sqrt . fromIntegral

inputType, outputType :: String
inputType  = "uint(size=8)"
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
