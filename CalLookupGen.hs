
module CalLookupGen (genFunArr) where

import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import Numeric


type Lookup = IntMap.IntMap String

genFunArr lower upper inputType outputType lookupFun = start ++ procBody ++ procEnd
    where
      start    = [" " ++ outputType ++ " lookupArr[" ++ show (upper-lower + 1) ++ "] = "  ++ showArr (genArr g) ++ ";",
                  " function lookup(" ++ inputType ++ " x) --> " ++ outputType ++ " :"]
      procBody = [" lookupArr[x-1]"]
      procEnd  = [" end"]

      genArr :: Lookup -> [String]
      genArr lookupKey = map (\i -> fromMaybe "" (IntMap.lookup i lookupKey)) [lower .. upper]

      f :: Lookup -> Int -> Lookup
      f mp elemKey =
          let lookupVal = lookupFun elemKey
          in IntMap.insert elemKey lookupVal mp

      g :: Lookup
      g = foldr (flip f) IntMap.empty [lower..upper]


showArr :: [String] -> String
showArr ss = "[" ++ concat (intersperse "," ss) ++ "]"
