{-# LANGUAGE FlexibleContexts #-}
module Main where

import Zelus
import Optimize
import VBool
import Plot
import Badness

import Data.Reflection
import Data.List( nub, sortOn )
import Test.QuickCheck
import Test.QuickCheck.Modifiers

--------------------------------------------------------------------------------
-- heater + controller

type Level = Double -- pump level
type Temp  = Double -- temperature

-- computing the weighted average

weigh :: Fractional a => [(a,a)] -> a
weigh axs = sum [ a*x | (a,x) <- axs ] / sum [ a | (a,_) <- axs ]

-- the plant

plant :: S Level -> S Temp
plant pump = roomTemp
 where
  startTemp    = outsideTemp
  boilerTemp   = 90
  heaterCoeff  = 0.1
  outsideTemp  = -5 -- (-5)
  outsideCoeff = 0.05
 
  -- the heater temperature is influenced by how much hot water is pumped into it
  -- and the room temperature
  heaterTemp = startTemp |-> weigh [ (1-pump,      heaterTemp)
                                   , (pump,        boilerTemp)
                                   , (heaterCoeff, roomTemp)
                                   ]

  -- the room temperature is influenced by the heater temperature and the outside
  -- temperature
  roomTemp   = startTemp |-> weigh [ (1,            roomTemp)
                                   , (heaterCoeff,  heaterTemp)
                                   , (outsideCoeff, outsideTemp)
                                   ]

-- controller

type Control = (Double, Double, Double)

controller :: Control -> S Temp -> S Temp -> S Level
controller (k_p,k_i,k_d) goalTemp roomTemp =
  (pump' `mn` 1) `mx` 0
 where
  err   = goalTemp - roomTemp
  pump' = val k_p * err
        + val k_i * integral err -- ((0 |> pre pump') >? 1 ? (0, err))
        + val k_d * deriv err

controlleR :: Control -> S Temp -> S Temp -> S Level
controlleR (k_p,k_i,k_d) goalTemp roomTemp =
  (pump' >=? 0) ? ((1 >=? pump') ? (pump', 1), 0)
 where
  err   = goalTemp - roomTemp
  pump' = val k_p * err
        + val k_i * integ (err `in1t` 0 `reset` (0 `when` changeGoalTemp))
        + val k_d * deriv err

  changeGoalTemp = abs (goalTemp - pre goalTemp) >? 1

cgood :: Control
--cgood = (3.997591176733649e-3,8.194771741046325e-5,5.618398605936785e-3)
--cgood = (5.0e-3,1.1446889636416996e-4,5.0e-3)
cgood = (1.2e-2,1.1446889636416996e-4,5.0e-3)

--------------------------------------------------------------------------------
-- properties

main = quickCheckWith stdArgs{ maxSuccess = 1000 } (withBadness prop_ReactFast)

data GoalTemp = GoalTemp [(Int,Temp)] (S Temp)
 deriving ( Eq, Ord )

goalTemp :: [(Int,Temp)] -> GoalTemp
goalTemp ds = GoalTemp ds (interp undefined ds)
 where
  interp t []          = repeat t
  interp _ ((n,t):nts) = replicate n t ++ interp t nts

instance Show GoalTemp where
  show (GoalTemp ds xs) = show ds

instance Arbitrary GoalTemp where
  arbitrary =
    do ds <- listOf (do t <- choose (15,25)
                        n <- choose (0,100)
                        return (n,t)) `suchThat` (not . null)
       return (goalTemp ds)

  shrink (GoalTemp ds _) =
    [ goalTemp ds'
    | ds' <- shrinkList (\_ -> []) ds ++ erase ds ++ smaller ds
    , not (null ds')
    , all (\(_,t) -> 10 <= t && t <= 30) ds'
    ]
   where
    erase [] = []
    erase (d@(n,t1):ds) =
      concat
      [ nub [ (n+m,t1):ds', (n+m,t2):ds' ]
      | (m,t2):ds' <- [ds]
      ] ++
      [ d : ds' | ds' <- erase ds ]

    smaller [] = []
    smaller ((n,t):ds) =
      [ (n',t):ds | n' <- shrink n ] ++
      [ (n,t'):ds | t' <- shrinkFloat t, 10 <= t' ] ++
      [ (n,t):ds' | ds' <- smaller ds ]

prop_Shrink (Fixed g@(GoalTemp _ _)) =
  g `notElem` take 1000 (xs ++ concatMap shrink xs)
 where
  xs = take 100 $ shrink g

--------------------------------------------------------------------------------

for' n = foldr1 (&&+) . take n

prop_ReactFast :: Given Badness => GoalTemp -> Property
prop_ReactFast (GoalTemp _ goalTemp) =
  whenFail (plot "failed" 300
            [ ("ok", graph (map howTrue ok))
            , ("goal",graph goalTemp)
            , ("room",graph roomTemp)
            ]) $
    for' tot ok
 where
  tot = 1000

  ok = (stableFor >=? 50) ? (zipWith (<=%) errTemp (val 1), val true)

  errTemp  = abs (goalTemp - roomTemp)
  roomTemp = plant pump
  pump     = controller cgood goalTemp roomTemp

  stableFor = n
   where
    --n = 1 |> (goalTemp ==? pre goalTemp ? (pre n+1,1))
    n = integ (1 `in1t` 1 `reset` (1 `when` (goalTemp /=? pre goalTemp)))

--------------------------------------------------------------------------------
-- show a given controller

display :: String -> (S Temp -> S Level) -> IO ()
display name controller =
  plot name 300
  [ ("room", graph roomTemp)
  , ("pump", graph (fmap (50*) pump))
  ]
 where
  roomTemp = plant pump
  pump     = controller roomTemp
 
--------------------------------------------------------------------------------
-- search

analyze :: Control -> (Integer, Double)
analyze c = stable 0 0 0 roomTemp
 where
  roomTemp = plant pump
  pump     = controller c 20 roomTemp

  stable m n k (t:ts)
    | k >= 100              = (n, m)
    | n >= 10000            = (n+1, m)
    | abs (t - 20) <= 0.01  = stable (m `max` t) n (k+1) ts
    | otherwise             = stable (m `max` t) (n+k+1) 0 ts

fit :: Control -> Double
fit c = fromInteger n / 100 + m
 where
  (n,m) = analyze c

--------------------------------------------------------------------------------
-- main

main1 :: IO ()
main1 =
  do putStrLn "-- a good controller --"
     print cgood
     print (analyze cgood)
     print (fit cgood)
     display "good" (controller cgood goalTemp)
 where
  goalTemp = repeat 20 -- replicate 60 20 ++ repeat 15

--------------------------------------------------------------------------------

