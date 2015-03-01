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
   $ cal-lookuptable-gen --function
   $ cal-lookuptable-gen --procedure
   $ cal-lookuptable-gen --array
-}

module Main where

import Control.Monad (when)
import qualified Data.IntMap as IntMap
import Data.Maybe
import System.Console.CmdArgs

---------------
-- user defined input range & user defined lookup function

lower, upper :: Integer
lower = 1
upper = 20

-- user defined lookup Integer -> Integer function with user defined range
lookupFun :: Integer -> Integer
lookupFun = round .  sqrt . fromInteger

---------------
-- code gen

type Lookup = IntMap.IntMap [Integer]

f :: Lookup -> Integer -> Lookup
f mp elemKey =
    let sqrVal = lookupFun elemKey
    in IntMap.alter (\a ->
                         if isNothing a
                         then Just [elemKey]
                         else
                             let xs = fromJust a
                             in Just $ elemKey : xs) (fromInteger sqrVal) mp

g :: Lookup
g = foldr (flip f) IntMap.empty [lower..upper]

genFunSt :: Integer -> Integer -> Int -> [String]
genFunSt lowerB upperB val =
  [" if (x >= " ++ show lowerB ++ " && x <= " ++ show upperB ++ ") then " ++ show val," else"]

genProcSt :: Integer -> Integer -> Int -> [String]
genProcSt lowerB upperB val =
  ["  if (x >= " ++ show lowerB ++ " && x <= " ++ show upperB ++ ") then val := " ++ show val ++ ";","  end"]

genIfElse :: Lookup -> (Integer -> Integer -> Int -> [String]) -> [String]
genIfElse lookupKey codeGenFun =
    let ls  = IntMap.toList lookupKey
    in concatMap (\(k,vs) -> codeGenFun (head vs) (last vs) k) ls

genProc :: [String]
genProc =
    let start    = [" int val;"," procedure lookupProc()"," begin"]
        ifThenElses = genIfElse g genProcSt
        procBody = ifThenElses
        procEnd  = [" end"]
    in start ++ procBody ++ procEnd

genFun :: [String]
genFun =
    let start    = [" function lookupFun(int x) --> int :"]
        ifThenElses = genIfElse g genFunSt
        ifThenElses' = init ifThenElses ++  [" else 0"] -- add value onto last else
        funBody = ifThenElses' ++ replicate (length ifThenElses') "  end"
        funEnd  = [" end"]
    in start ++ funBody ++ funEnd

genArr :: Lookup -> [Integer]
genArr lookupKey =
    let ls  = IntMap.toList lookupKey
        valToKeyList = concatMap (\(k,vs) -> map (\v -> (fromInteger v,toInteger k)) vs) ls
        valToKeyMap = IntMap.fromList valToKeyList :: IntMap.IntMap Integer
        arrayLookup = map (\k ->
                           if IntMap.member k valToKeyMap
                           then fromJust $ IntMap.lookup k valToKeyMap
                           else -1) [fromInteger lower .. fromInteger upper]
    in arrayLookup

genProcArr :: [String]
genProcArr =
    let start    = [" int lookupVal;",
                    " int lookupArr[" ++ show (upper-lower + 1) ++ "] := "  ++ show (genArr g) ++ ";",
                    " procedure lookupProcArr(int x) begin"]
        procBody = [" lookupVal := lookupArr[x-1];"]
        procEnd  = [" end"]
    in start ++ procBody ++ procEnd

main :: IO ()
main = do
  opts <- getOpts
  when (function opts) (mapM_ putStrLn genFun)
  when (procedure opts) (mapM_ putStrLn genProc)
  when (array opts) (mapM_ putStrLn genProcArr)

---------------------
-- command line options

data CalLookupOptions = CalLookupOptions
    { function :: Bool
    , procedure :: Bool
    , array :: Bool
    } deriving (Data, Typeable, Show, Eq)

calLookupOpts :: CalLookupOptions
calLookupOpts = CalLookupOptions
    { function = def &= help "generates a function lookup with if/else statements"
    , procedure = def &= help "generates a procedure lookup with if/end statements"
    , array = def &= help "generates a procedure lookup with array indexing"
    }

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
