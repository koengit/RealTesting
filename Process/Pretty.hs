-- Pretty-printing.
module Process.Pretty where

import qualified Data.Map.Strict as Map
import Text.PrettyPrint.HughesPJClass hiding ((<>), double)
import qualified Text.PrettyPrint.HughesPJClass
import Text.Printf
import Process.Language

----------------------------------------------------------------------
-- Pretty-printing
----------------------------------------------------------------------

instance Show Process where
  show = show . pPrint

instance Show Step where
  show = show . pPrint

instance Show Expr where
  show = show . pPrint

instance Show Var where
  show = show . pPrint

instance Pretty Process where
  pPrint p =
    vcat $
      [ hang (text "initial") 2 (pPrint (start p)) | nonempty (start p) ] ++
      [ hang (text "timestep") 2 (pPrint (step p)) | nonempty (step p) ]
    where
      nonempty (Update m) = Map.size m > 0
      nonempty _ = True

instance Pretty Step where
  pPrint (If e s1 s2) =
    ppIfThenElse (pPrint e) (pPrint s1) (pPrint s2)
  pPrint (Assume msg e s) =
    hang (text "assume" <+> text (show msg)) 2 (pPrint e) $$
    pPrint s
  pPrint (Assert msg e s) =
    hang (text "assert" <+> text (show msg)) 2 (pPrint e) $$
    pPrint s
  pPrint (Update m)
    | Map.null m = text "skip"
    | otherwise =
      vcat
        [ hang (pPrint x <+> text "<-") 2 (pPrint e)
        | (x, e) <- Map.toList m ]

instance Pretty Expr where
  pPrintPrec _ p = ppExp p

instance Pretty Var where
  pPrint (Global x) = text x
  pPrint (Local n) = text "x" <#> pPrint n

instance Pretty Doc where
  pPrint = id

-- Precedence levels:
-- 0: no brackets
-- 1: and (associative)
-- 2: not
-- 3: positive/zero
-- 4: plus (associative)
-- 5: negate
-- 6: times (associative)
-- 7: power (non-associative)
-- 9: atomic
ppExp :: Rational -> Expr -> Doc
ppExp _ (Var x) = pPrint x
ppExp _ Delta = text "dt"
ppExp _ (Double x) = text (shortest (show x) (printf "%.5f" x))
  where
    shortest x y
      | length x <= length y = x
      | otherwise = y
ppExp n (Plus e1 e2) =
  maybeParens (n > 4) $
    fsep $
      (case e1 of Negate{} -> ppTerm "-" e1; _ -> ppExp 4 e1):
      map (ppTerm "+") pos ++
      map (ppTerm "-") neg
  where
    (pos, neg) = terms e2
    ppTerm s e = text s <+> ppExp 4 e
ppExp n e@Times{} =
  maybeParens (n > 6) $
  fsep $ punctuate (text " *") $
  map (ppExp 7) $
    [Double k | k /= 1 || null es] ++ es
  where
    (k, es) = factors e
ppExp n (Power e (Double (-1))) =
  ppUnary n "1/" 7 e
ppExp n (Power e1 e2) =
  maybeParens (n > 7) $
    cat [ppExp 8 e1 <#> text "^", nest 2 (ppExp 8 e2)]
ppExp n (Negate e) = ppUnary n "-" 5 e
ppExp n (Not (And e1 e2)) =
  ppNonAssoc n "or" 1 (neg e1) (neg e2)
  where
    neg (Not x) = x
    neg x = Not x
ppExp n (Not e) = ppUnary n "not " 2 e
ppExp n (And e1 e2) = ppNonAssoc n "and" 1 e1 e2
ppExp _ (Bool True) = text "true"
ppExp _ (Bool False) = text "false"
ppExp n (Positive e) =
  ppAssoc n ">=" 3 (ppSum pos) (ppSum neg)
  where
    (pos, neg) = terms e
ppExp n (Zero e) =
  ppAssoc n "=" 3 (ppSum pos) (ppSum neg)
  where
    (pos, neg) = terms e
ppExp n (Cond e1 e2 e3) =
  maybeParens (n > 0) $
    -- else-branch must be atomic to avoid ambiguity
    ppIfThenElse (ppExp 0 e1) (ppExp 0 e2) (ppExp 9 e3)
ppExp _ (Primitive _ name es) =
  ppFunction name es

ppFunction :: String -> [Expr] -> Doc
ppFunction op es =
  cat [
    text op,
    nest 2 $ parens $
      sep (punctuate comma (map (ppExp 0) es))]

ppUnary :: Rational -> String -> Rational -> Expr -> Doc
ppUnary n op p e =
  maybeParens (n > p) $
    text op <#> ppExp (p+1) e

ppAssoc :: Rational -> String -> Rational -> Expr -> Expr -> Doc
ppAssoc n op p e1 e2 =
  maybeParens (n > p) $
    sep [ppExp p e1, text op <+> ppExp p e2]

ppNonAssoc :: Rational -> String -> Rational -> Expr -> Expr -> Doc
ppNonAssoc n op p e1 e2 =
  maybeParens (n > p) $
    sep [ppExp (p+1) e1, text op <+> ppExp (p+1) e2]

ppSum :: [Expr] -> Expr
ppSum [] = Double 0
ppSum xs = foldr1 Plus xs

ppIfThenElse :: Doc -> Doc -> Doc -> Doc
ppIfThenElse x y z =
  sep [
    sep [text "if", nest 2 x, text "then"],
    nest 2 y,
    text "else",
    nest 2 z]

infixl 6 <#>
(<#>) :: Doc -> Doc -> Doc
(<#>) = (Text.PrettyPrint.HughesPJClass.<>)
