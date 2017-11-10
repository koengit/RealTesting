module Plot where

import Data.List( intersperse )
import System.Process( system )

import Zelus

--------------------------------------------------------------------------------
-- plotting streams

graph :: S Double -> (S Double, S Double)
graph y = ([0..], y)

plot :: FilePath -> Int -> [(String,(S Double,S Double))] -> IO ()
plot file n xys =
  do sequence_
       [ writeFile (file ++ "_" ++ name ++ "_.xy") $ unlines $
           [ show x ++ " " ++ show y
           | (x,y) <- take n (uncurry zip xy)
           ]
       | (name,xy) <- xys
       ]
     writeFile (file ++ "_gnuplot_.in") $ unlines $
       [ "set terminal pdf enhanced font 'Times,18' lw 3"
       , "set grid"
       , "set output '" ++ file ++ "_plot_.pdf'"
       , "plot " ++
           concat (intersperse ", "
           [ "'" ++ file ++ "_" ++ name ++ "_.xy' with lines title '" ++ name ++ "'"
           | (name,_) <- xys
           ])
       ]
     system ("gnuplot < '" ++ file ++ "_gnuplot_.in'")
     return ()
     
--------------------------------------------------------------------------------

