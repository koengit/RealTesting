-- http://www.sfu.ca/~ssurjano/optimization.html
module TestFunctions where

import Graphics.Gnuplot.Simple
import Optimize

data TestFunction =
  TestFunction {
    inputSpace :: ([Double], [Double]),
    best :: [Double],
    eval :: [Double] -> Double }

dimensions :: TestFunction -> Int
dimensions = length . fst . inputSpace

globalMinimum :: TestFunction -> ([Double], Double)
globalMinimum func = (xs, eval func xs)
  where
    xs = best func

sumsq :: [Double] -> Double
sumsq = sum . map (^2)

double :: Int -> Double
double = fromIntegral

plot :: TestFunction -> IO ()
plot func =
  case dimensions func of
    1 ->
      let ([xmin], [xmax]) = inputSpace func in
      plotFunc [] (linearScale 1000 (xmin, xmax)) (\x -> eval func [x])
    2 ->
      let ([xmin, ymin], [xmax, ymax]) = inputSpace func
          colourMap = False in
      plotFunc3d [] [Plot3dType ColorMap | colourMap]
        (linearScale 100 (xmin, xmax))
        (linearScale 100 (ymin, ymax))
        (\x y -> eval func [x, y])
    _ ->
      error "can only plot functions of dimensions 1 or 2"

optimise :: TestFunction -> ([Double], Double)
optimise func =
    -- Start one quarter of the way into the input space,
    -- the middle is often the global minimum
    optimise' 0.25 func

optimise' :: Double -> TestFunction -> ([Double], Double)
optimise' w func = (xs, y)
  where
    (xs, y, _) =
      last . take n . minimize ds ps $ eval func
    (lo, hi) = inputSpace func

    weight x y = (1-w)*x + w*y

    ps = zipWith weight lo hi
    ds = zipWith (-) lo hi

    n = 1000

limit :: TestFunction -> TestFunction
limit func =
  func { eval = f }
  where
    (lo, hi) = inputSpace func

    f xs
      | or [x < y | (x, y) <- zip xs lo] = 1000000
      | or [x > y | (x, y) <- zip xs hi] = 1000000
      | otherwise = eval func xs

-- http://www.sfu.ca/~ssurjano/ackley.html.
-- "It is characterized by a nearly flat outer region, and a large hole at the centre."
-- Many poor-quality local minima.
ackley d =
  TestFunction {
    inputSpace = (replicate d (-32.768), replicate d 32.768),
    best = replicate d 0,
    eval = f }
  where
    f xs =
      -a * exp (-b * sqrt (sumsq xs / dd))
      - exp (sum [ cos (c*x) | x <- xs ] / dd)
      + a + exp 1
    a = 20
    b = 0.2
    c = 2*pi
    dd = double d

-- http://www.sfu.ca/~ssurjano/drop.html
-- "Multimodal and highly complex."
dropwave =
  TestFunction {
    inputSpace = ([-5.12, -5.12], [5.12, 5.12]),
    best = [0, 0],
    eval = f }
  where
    f [x, y] =
      -(1 + cos (12 * sqrt norm))/(0.5 * norm + 2)
      where
        norm = x^2 + y^2

-- http://www.sfu.ca/~ssurjano/egg.html
-- "The Eggholder function is a difficult function to optimize, because of the large number of local minima."
eggholder =
  TestFunction {
    inputSpace = ([-512, -512], [512, 512]),
    best = [512, 404.2319],
    eval = f }
  where
    f [x, y] =
      -(y+47) * sin (sqrt (abs (y + x/2 + 47)))
      - x * sin (sqrt (abs (x - (y + 47))))

-- http://www.sfu.ca/~ssurjano/grlee12.html
-- "A simple one-dimensional test function."
gramacyLee =
  TestFunction {
    inputSpace = ([0.5], [2.5]),
    best = [0.54856],
    eval = f }
  where
    f [x] =
      sin (10*pi*x) / (2*x) + (x-1)^4

-- http://www.sfu.ca/~ssurjano/griewank.html
-- "Many widespread local minima, which are regularly distributed."
griewank d =
  TestFunction {
    inputSpace = (replicate d (-600), replicate d 600),
    best = replicate d 0,
    eval = f }
  where
    f xs =
      1 + sum (map (^2) xs) / 4000 -
      product [ cos (x / sqrt i) | (x, i) <- zip xs [1..] ]

-- http://www.sfu.ca/~ssurjano/camel6.html
sixHumpCamel =
  TestFunction {
    inputSpace = ([-3, -2], [3, 2]),
    best = [0.0898, -0.7126],
    eval = f }
  where
    f [x, y] =
      (4 - 2.1*x^2 + x^4/3) * x^2 +
      x * y +
      (-4 + 4*y^2) * y^2

-- http://www.sfu.ca/~ssurjano/michal.html
michalewicz d =
  TestFunction {
    inputSpace = (replicate d 0, replicate d pi),
    best = error "don't know optimum",
    eval = f }
  where
    f xs =
      -sum [sin x * (sin (i * x^2 / pi))^(2*m) | (x, i) <- zip xs [1..]]
    m = 10

-- http://www.sfu.ca/~ssurjano/schwef.html
schwefel d =
  TestFunction {
    inputSpace = (replicate d (-500), replicate d 500),
    best = replicate d 420.9687,
    eval = f }
  where
    f xs =
      418.9829 * dd
      - sum [ x * sin (sqrt (abs x)) | x <- xs ]
    dd = double d
