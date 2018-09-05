module Parse(parseFlow, parseFile) where

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Perm
import Text.Parsec.Expr
import qualified Lex as Lex
import Model
import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad
import System.FilePath.Glob
import Data.Either

type Parser = Parsec [Lex.Token] ()

tok :: (Lex.Token -> Maybe a) -> Parser a
tok p = do
  filename <- sourceName <$> getPosition
  let
    toSourcePos (Lex.AlexPn _ l c) = newPos filename l c
    p' (Lex.Error info) = Just (Left info)
    p' tok = Right <$> p tok
  
  x <- token (quote . Lex.tokenString . Lex.tokenInfo)
    (toSourcePos . Lex.tokenPosition . Lex.tokenInfo) p'
  case x of
    Left _ -> fail "lexical error"
    Right y -> return y

quote s = "'" ++ s ++ "'"

symbol :: String -> Parser ()
symbol sym = tok p <?> quote sym
  where
    p x | Lex.tokenString (Lex.tokenInfo x) == sym = Just ()
    p _ = Nothing

num :: Parser Double
num = tok p <?> "number"
  where
    p (Lex.Num x _) = Just x
    p _ = Nothing

var :: Parser Var
var = tok p <?> "variable"
  where
    p (Lex.Var x _) = Just x
    p _ = Nothing

negVar :: Parser Var
negVar = tok p <?> "negated variable"
  where
    p (Lex.NegVar x _) = Just x
    p _ = Nothing

braces :: Parser a -> Parser a
braces p = symbol "{" *> p <* symbol "}"

section :: String -> Parser a -> Parser a
section s p = symbol s *> braces p

modeSection :: Parser a -> Parser (ModeName, a)
modeSection p = do
  x <- var
  y <- braces p
  return (x, y)

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

square :: Parser a -> Parser a
square p = symbol "[" *> p <* symbol "]"

parseModel :: Parser Model
parseModel = ((continuous <|> hybrid) <* eof) >>= check
  where
    check (Left msg) = fail msg
    check (Right x) = return x

    continuous = do
      symbol "continuous"
      symbol "reachability"
      f <- braces $ permute $ assembleContinuous
        <$?> ((), parseSettings)
        <|?> (Set.empty, parseVars)
        <|?> (Map.empty, parseParams)
        <||> parseDerivatives
        <|?> ([], parseInvariant)
        <|?> ([], section "init" parseConstraints)
      x <- option [] (symbol "unsafe" *> symbol "set" *> braces parseConstraints)
      return (f x)

    assembleContinuous settings vars params derivatives invariant constraints unsafe =
      assemble settings vars params (Map.singleton "" (derivatives, invariant)) [] ("", constraints) (Map.singleton "" unsafe)

    hybrid = do
      symbol "hybrid"
      symbol "reachability"
      f <- braces $ permute $ assemble
        <$?> ((), parseSettings)
        <|?> (Set.empty, parseVars)
        <|?> (Map.empty, parseParams)
        <||> parseModes
        <|?> ([], parseJumps)
        <||> parseInit
      x <- option Map.empty parseUnsafe
      return (f x)

parseSettings :: Parser ()
parseSettings =
  section "setting" matchedBraces

matchedBraces :: Parser ()
matchedBraces = skipMany (tok p <|> braces matchedBraces)
  where
    p (Lex.LBrace _) = Nothing
    p (Lex.RBrace _) = Nothing
    p _ = Just ()

parseVars :: Parser (Set Var)
parseVars = Set.fromList <$> do
  symbol "state"
  symbol "var"
  sepBy1 var (symbol ",")

parseParams :: Parser (Map Var Double)
parseParams = do
  symbol "constants"
  toMap "parameter" $ flip sepBy1 (symbol ",") $ do
    x <- var
    symbol "="
    k <- num
    return (x, k)

toMap :: Ord a => String -> Parser [(a, b)] -> Parser (Map a b)
toMap kind p = do
  xs <- p
  let m = Map.fromList xs
  when (Map.size m /= length xs) $
    fail ("the same " ++ kind ++ " was defined twice")
  return m

parseModes :: Parser (Map ModeName (Map Var Expr, [Constraint]))
parseModes =
  section "modes" $ toMap "mode" $ many $ modeSection $ permute $
    (,) <$?> (Map.empty, parseDerivatives)
        <|?> ([], parseInvariant)

parseInit :: Parser (ModeName, [Constraint])
parseInit = section "init" (modeSection parseConstraints)

parseConstraints :: Parser [Constraint]
parseConstraints = many $ try interval <|> constraint
  where
    interval = do
      x <- parseExpr
      symbol "in"
      range <- square $ do
        lo <- num
        symbol ","
        hi <- num
        return (lo, hi)
      return (x `In` range)
    constraint = do
      x <- parseExpr
      op <- msum [
        symbol "=" *> return Zero,
        symbol ">=" *> return Positive,
        symbol "<=" *> return (Positive . Negate)]
      y <- parseExpr
      return (op (Plus x (Negate y)))

parseInvariant :: Parser [Constraint]
parseInvariant = section "inv" parseConstraints

parseDerivatives :: Parser (Map Var Expr)
parseDerivatives = do
  prefix
  try (braces num) <|> return 0
  braces $ toMap "derivative" $ many $ do
    x <- var
    symbol "'"
    symbol "="
    y <- parseExpr
    return (x, y)
  where
    prefix = msum [
      symbol "poly" *> symbol "ode" *> num *> pure (),
      symbol "nonpoly" *> symbol "ode",
      symbol "lti" *> symbol "ode"]

parseJumps :: Parser ([(ModeName, Jump)])
parseJumps =
  section "jumps" $ many $ do
    from <- var
    symbol "->"
    to <- var
    let jump xs ys () = Jump to xs ys
    j <- permute $ jump
      <$?> ([], section "guard" parseConstraints)
      <|?> (Map.empty, section "reset" reset)
      <|?> ((), skipMany1 extra)
    return (from, j)
  where
    extra =
      msum [
        symbol "interval" *> symbol "aggregation",
        symbol "parallelotope" *> symbol "aggregation" *> braces matchedBraces]
    reset =
      toMap "variable" $ many $ do
        x <- var
        symbol "'"
        symbol ":="
        y <- parseExpr
        return (x, y)

parseUnsafe :: Parser (Map ModeName [Constraint])
parseUnsafe = do
  symbol "unsafe"
  symbol "set"
  braces $ toMap "unsafe set" $ many $ modeSection parseConstraints

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table term <?> "expression"
  where
    term =
      parens parseExpr <|>
      unary "exp" (Power (Const (exp 1))) <|>
      unary "sin" Sin <|>
      unary "cos" (\x -> Sin (Plus x (Const (3*pi/2)))) <|>
      unary "sqrt" (\x -> Power x (Const (1/2))) <|>
      interval <|>
      (Var <$> var) <|>
      (Negate <$> Var <$> negVar) <|>
      (Const <$> num)
      <?> "term"
    table =
      [[Prefix (symbol "-" *> return Negate)],
       [Infix (symbol "^" *> return Power) AssocRight],
       [Infix (symbol "/" *> return (\x y -> Times x (Power y (Const (-1))))) AssocLeft],
       [Infix (symbol "*" *> return Times) AssocRight],
       [Infix (symbol "-" *> return (\x y -> Plus x (Negate y))) AssocLeft],
       [Infix (symbol "+" *> return Plus) AssocRight]]
    unary sym op = do
      symbol sym
      op <$> parens parseExpr
    interval = square $ do
      x <- num
      symbol ","
      y <- num
      return (Interval x y)

assemble :: () -> Set Var -> Map Var Double -> Map ModeName (Map Var Expr, [Constraint]) -> [(ModeName, Jump)] -> (ModeName, [Constraint]) -> Map ModeName [Constraint] -> Either String Model
assemble () vars_ params_ modes_ jumps_ (initialMode_, initialVars_) unsafe_
  | not (Set.null (Set.intersection vars_ (Map.keysSet params_))) =
    Left "the same variable is used both as a parameter and as a state variable"
  | any (`Map.notMember` modes_) (map fst jumps_) =
    Left "jump has nonexistent source"
  | any (`Map.notMember` modes_) (map (target . snd) jumps_) =
    Left "jump has nonexistent target"
  | initialMode_ `Map.notMember` modes_ =
    Left "initial mode does not exist"
  | any (`Map.notMember` modes_) (Map.keys unsafe_) =
    Left "unsafe set given for nonexistent node"
  | any (`Set.notMember` vars_) (varsModel model) =
    Left "unbound variable"
  | otherwise =
    Right model
  where
    model =
      substModel params_ $
      Model {
        vars = vars_,
        modes = Map.mapWithKey toMode modes_,
        initialMode = initialMode_,
        initialVars = initialVars_ }
    toMode mode (derivatives, invariant) =
      Mode {
        invariant = invariant,
        derivatives = derivatives,
        jumps = [jump | (mode', jump) <- jumps_, mode == mode'],
        unsafe = Map.findWithDefault [] mode unsafe_ }

    varsModel m =
      concatMap varsMode (Map.elems (modes m)) ++
      concatMap varsConstraint (initialVars m)
    varsMode m =
      concatMap varsConstraint (invariant m) ++
      Map.keys (derivatives m) ++
      concatMap varsExpr (Map.elems (derivatives m)) ++
      concatMap varsJump (jumps m) ++
      concatMap varsConstraint (unsafe m)
    varsJump m =
      concatMap varsConstraint (condition m) ++
      Map.keys (reset m) ++
      concatMap varsExpr (Map.elems (reset m))
    varsConstraint (Zero e) =
      varsExpr e
    varsConstraint (Positive e) =
      varsExpr e
    varsConstraint (e `In` _) =
      varsExpr e
    varsExpr (Var x) = [x]
    varsExpr (Const _) = []
    varsExpr (Plus e1 e2) = varsExpr e1 ++ varsExpr e2
    varsExpr (Times e1 e2) = varsExpr e1 ++ varsExpr e2
    varsExpr (Power e1 e2) = varsExpr e1 ++ varsExpr e2
    varsExpr (Negate e) = varsExpr e
    varsExpr (Sin e) = varsExpr e
    varsExpr (Interval x y) = []

    substModel sub m =
      m {
        modes = Map.map (substMode sub) (modes m),
        initialVars = map (substConstraint sub) (initialVars m) }
    substMode sub m =
      m {
        invariant = map (substConstraint sub) (invariant m),
        derivatives = Map.map (subst sub) (derivatives m),
        jumps = map (substJump sub) (jumps m),
        unsafe = map (substConstraint sub) (unsafe m) }
    substJump sub j =
      j {
        condition = map (substConstraint sub) (condition j),
        reset = Map.map (subst sub) (reset j) }
    substConstraint sub (Zero e) = Zero (subst sub e)
    substConstraint sub (Positive e) = Positive (subst sub e)
    substConstraint sub (e `In` range) = subst sub e `In` range
    subst sub (Var x) =
      case Map.lookup x sub of
        Just y -> Const y
        Nothing -> Var x
    subst sub (Const x) = Const x
    subst sub (Plus e1 e2) = Plus (subst sub e1) (subst sub e2)
    subst sub (Times e1 e2) = Times (subst sub e1) (subst sub e2)
    subst sub (Power e1 e2) = Power (subst sub e1) (subst sub e2)
    subst sub (Negate e) = Negate (subst sub e)
    subst sub (Sin e) = Sin (subst sub e)
    subst sub (Interval x y) = Interval x y

parseFlow :: FilePath -> String -> Either ParseError Model
parseFlow file contents =
  parse parseModel file (Lex.lexFlow contents)

parseFile :: FilePath -> IO Model
parseFile file = do
  contents <- readFile file
  case parseFlow file contents of
    Left err -> do
      print err
      error "Parse failed!"
    Right x -> return x

test :: IO ()
test = glob "examples/*.model" >>= mapM_ parseFile
