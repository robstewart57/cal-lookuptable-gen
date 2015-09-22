
module CalLookupGen (genFunArr) where

import qualified Data.IntMap as IntMap
import Data.List
import Data.List.Split
import Data.Maybe
import Numeric


type Lookup = IntMap.IntMap String

genFunArr lower upper inputType outputType lookupFun = start ++ funBody ++ funEnd
    where
      start    = [" " ++ outputType ++ " lookupArr[" ++ show (upper-lower + 1) ++ "] = "  ++ showArr (genArr g) ++ ";",
                  " function lookup(" ++ inputType ++ " x) --> " ++ outputType ++ " :"]
      funBody = [" lookupArr[x-1]"]
      funEnd  = [" end"]

      genArr :: Lookup -> [String]
      genArr lookupKey = map (\i -> fromMaybe "" (IntMap.lookup i lookupKey)) [lower .. upper]

      f :: Lookup -> Int -> Lookup
      f mp elemKey =
          let lookupVal = lookupFun elemKey
          in IntMap.insert elemKey lookupVal mp

      g :: Lookup
      g = foldr (flip f) IntMap.empty [lower..upper]


showArr :: [String] -> String
showArr ss = "[" ++ concat xs ++ concat lastLine ++ "]"
    where
      elems = take (length xs - 1) (concat xs)
      sss = chunksOf 20 ss :: [[String]]
      xs  = concatMap (\ ss -> intersperse "," ss ++ [",\n"]) (init sss)
      lastLine = intersperse "," (last sss :: [String])
