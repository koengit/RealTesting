module Plot where

import Data.List( intercalate )
import System.Process( system )
import System.Directory( getCurrentDirectory )
import System.FilePath( (</>) )
import Control.Arrow

import Zelus

--------------------------------------------------------------------------------
-- plotting streams

graph :: S Double -> (S Double, S Double)
graph y = ([0..], y)

plot :: FilePath -> Int -> [[(String,(S Double,S Double))]] -> IO ()
plot file n xyss0 =
  do let xyss :: [[(String,(S Double,S Double))]]
         xyss = map (map (id *** (take n *** take n))) xyss0
     dir <- getCurrentDirectory
     sequence_
       [ writeFile (dir </> file ++ "_" ++ name ++ "_.xy") $ unlines $
           [ show x ++ " " ++ show y
           | (x,y) <- take n (uncurry zip xy)
           ]
       | xys <- xyss,
         (name,xy) <- xys
       ]
     writeFile (dir </> file ++ "_gnuplot_.in") $ unlines $
       [ "set terminal pdf enhanced font 'Times,18' lw 3"
       , "set grid"
       , if any (< 0) (concatMap (snd . snd) . concat $ xyss) then "" else "set yrange [0:]"
       , "set output '" ++ file ++ "_plot_.pdf'" ] ++
       [ "plot " ++
         intercalate ","
           [ "'" ++ file ++ "_" ++ name ++ "_.xy' with lines title '" ++ name ++ "'"
           | (name, xy) <- xys ]
       | xys <- xyss ]
     system ("gnuplot < \"" ++ (dir </> file ++ "_gnuplot_.in\""))
     return ()
     
--------------------------------------------------------------------------------

