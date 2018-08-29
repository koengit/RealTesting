{
module Lex (lexFlow, AlexPosn(..), Token(..), Info(..)) where

import Control.Monad
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z_]
$dot = \.
@ident = $alpha [$alpha $digit $dot]*
@numeral = $digit+
@decimal = @numeral ($dot @numeral)? | @numeral? $dot @numeral
@float = [\-\+]? @decimal ([eE] [\-\+]? @numeral)?

tokens :-
  $white+ ;
  "#".* ;
  "-" @ident {consume (\info -> NegVar (drop 1 (tokenString info)) info)}
  @ident {consume (\info -> Var (tokenString info) info)}
  @float {consume (\info -> num info)}
  "{" {consume LBrace}
  "}" {consume RBrace}
  "[" {consume LSquare}
  "]" {consume RSquare}
  "(" {consume LParen}
  ")" {consume RParen}
  "'" {consume Prime}
  "+" {consume Plus}
  "-" {consume Minus}
  "*" {consume Times}
  "/" {consume Divide}
  "^" {consume Exp}
  "," {consume Comma}
  ":" {consume Colon}
  ":=" {consume Assign}
  "=" {consume Equals}
  ">=" {consume GreaterEquals}
  "<=" {consume LessEquals}
  ">" {consume Greater}
  "<" {consume Less}
  "->" {consume Arrow}

{
data Token =
  Num { numValue :: Double, tokenInfo :: Info } |
  Var { varName :: String, tokenInfo :: Info } |
  NegVar { varName :: String, tokenInfo :: Info } |
  LBrace { tokenInfo :: Info } |
  RBrace { tokenInfo :: Info } |
  LSquare { tokenInfo :: Info } |
  RSquare { tokenInfo :: Info } |
  LParen { tokenInfo :: Info } |
  RParen { tokenInfo :: Info } |
  Comma { tokenInfo :: Info } |
  Colon { tokenInfo :: Info } |
  Assign { tokenInfo :: Info } |
  Equals { tokenInfo :: Info } |
  LessEquals { tokenInfo :: Info } |
  GreaterEquals { tokenInfo :: Info } |
  Less { tokenInfo :: Info } |
  Greater { tokenInfo :: Info } |
  Arrow { tokenInfo :: Info } |
  Prime { tokenInfo :: Info } |
  Plus { tokenInfo :: Info } |
  Minus { tokenInfo :: Info } |
  Times { tokenInfo :: Info } |
  Divide { tokenInfo :: Info } |
  Exp { tokenInfo :: Info } |
  Error { tokenInfo :: Info }
  deriving Show

data Info =
  Info { tokenString :: String, tokenPosition :: AlexPosn }
  deriving Show

num :: Info -> Token
num info =
  case readNum (tokenString info) of
    Nothing -> Error info
    Just x -> Num x info

readNum :: String -> Maybe Double
readNum ('+':s) = readNum s
readNum ('-':s) = negate <$> readNum s
readNum s@('.':_) = readNumPrim ('0':s)
readNum s = readNumPrim s

readNumPrim :: String -> Maybe Double
readNumPrim s =
  case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

consume :: (Info -> Token) -> AlexPosn -> String -> Token
consume f p s = f (Info s p)

lexFlow :: String -> [Token]
lexFlow = alexScanTokens
}
