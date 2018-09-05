
import VBool
import Data
import Optimize

qsort :: Ord a => [a] -> (Int,[a])
qsort []  = (0,[])
qsort xs' = (b+n0+n1+n2+m0+m2, ys0++(x:xs1)++ys2)
 where
  (n0,xs0) = filt (<x)  xs
  (n1,xs1) = filt (==x) xs
  (n2,xs2) = filt (>x)  xs
  (m0,ys0) = qsort xs0
  (m2,ys2) = qsort xs2

  filt p xs = (length xs,filter p xs)

  (b,x,xs) = medianOfThree xs'

medianOfThree [x]   = (0,x,[])
medianOfThree [x,y] = (0,x,[y])
--medianOfThree (x:xs) = (0,x,xs)
medianOfThree xs    =
  if x0 <= x1 then
    if x2 <= x0 then
      (2, x0, tail xs)
    else if x2 <= x1 then
      (3, x2, init xs)
    else
      (3, x1, take k xs ++ drop (k+1) xs)
  else
    if x2 <= x1 then
      (2, x1, take k xs ++ drop (k+1) xs)
    else if x2 <= x0 then
      (3, x2, init xs)
    else
      (3, x0, tail xs)
 where
  k   = length xs `div` 2
  x0  = head xs
  x1  = xs!!k
  x2  = last xs

worstCase :: (Data a, Ord b) => a -> (a -> b) -> (a,b)
worstCase x0 f = (fill x0 ws,ans)
 where
  (ws,ans,_) = last
             . giveUp 50
             . take 1000
             . minimize (repeat 15) (vals x0)
             $ f . fill x0

main =
  sequence_
  [ print (k,n,fromIntegral n / fromIntegral k)
  | k <- [1..100]
  , let n = -snd (worstCase (replicate k (0::Int)) f)
  ]
 where
  f xs = -n
   where
    (n,ys) = qsort xs
