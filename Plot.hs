module Plot where

import Data.List( intersperse )
import System.Process( system )
import System.Directory( getCurrentDirectory )
import System.FilePath( (</>) )

import Zelus

--------------------------------------------------------------------------------
-- plotting streams

graph :: S Double -> (S Double, S Double)
graph y = ([0..], y)

plot :: FilePath -> Int -> [(String,(S Double,S Double))] -> IO ()
plot file n xys =
  do dir <- getCurrentDirectory
     sequence_
       [ writeFile (dir </> file ++ "_" ++ name ++ "_.xy") $ unlines $
           [ show x ++ " " ++ show y
           | (x,y) <- take n (uncurry zip xy)
           ]
       | (name,xy) <- xys
       ]
     writeFile (dir </> file ++ "_gnuplot_.in") $ unlines $
       [ "set terminal pdf enhanced font 'Times,18' lw 3"
       , "set grid"
       , "set output '" ++ file ++ "_plot_.pdf'"
       , "plot " ++
           concat (intersperse ", "
           [ "'" ++ file ++ "_" ++ name ++ "_.xy' with lines title '" ++ name ++ "'"
           | (name,_) <- xys
           ])
       ]
     system ("gnuplot < '" ++ (dir </> file ++ "_gnuplot_.in'"))
     return ()
     
--------------------------------------------------------------------------------

