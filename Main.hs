{-# LANGUAGE DeriveDataTypeable  #-}

{-
 This program generates an Integer -> Integer lookup table for
 the CAL programming language. This might be useful if computing
 the lookup function on an FPGA is too intensive.

 To use, define the lower and upper bounds of the lookup and also
 define the lookup functin. Comment out in `main` whether you want
 a procedure or a function CAL implementation of the lookup table.

 To use with the Haskell platform:
   $ cabal install
   $ cal-lookuptable-gen
-}

module Main where

import Control.Monad (when,void)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import Numeric
import System.Console.CmdArgs

---------------
-- user defined input range & user defined lookup function

lower, upper :: Int
lower = 1
upper = 10

-- user defined lookup Integer -> Integer function with user defined range
lookupFun :: Int -> String
lookupFun = show . round .  sqrt . fromIntegral


---------------
-- code gen

type Lookup = IntMap.IntMap String

f :: Lookup -> Int -> Lookup
f mp elemKey =
    let lookupVal = lookupFun elemKey
    in IntMap.insert elemKey lookupVal mp

g :: Lookup
g = foldr (flip f) IntMap.empty [lower..upper]

genArr :: Lookup -> [String]
genArr lookupKey = map (\i -> fromMaybe "" (IntMap.lookup i lookupKey)) [lower .. upper]

genFunArr :: [String]
genFunArr =
    let start    = [" int lookupArr[" ++ show (upper-lower + 1) ++ "] = "  ++ showArr (genArr g) ++ ";",
                    " function lookup(int x) --> int :"]
        procBody = [" lookupArr[x-1]"]
        procEnd  = [" end"]
    in start ++ procBody ++ procEnd

showArr :: [String] -> String
showArr ss = "[" ++ concat (intersperse "," ss) ++ "]"

main :: IO ()
main = do
  void getOpts -- for --help option
  mapM_ putStrLn genFunArr

-- ---------------------
-- -- command line options

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
