module Lambda exposing (Name, Expr(..), DeBruijn(..), Lit(..), deBruijnBeta, toDeBruijn, toExpr,
  equivalent)

import Char exposing (fromCode, isUpper, isLower, toCode)
import Dict exposing (Dict)
import List.Extra exposing ((!!), foldl1)

import Common exposing (fromJust)

type alias Name = String

type Expr =
    Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit

type DeBruijn =
    DVar Int
  | DApp DeBruijn DeBruijn
  | DLam DeBruijn
  | DLit Lit

type Lit =
    LInt Int
  | LBool Bool

(<$>) : (a -> b) -> Maybe a -> Maybe b
(<$>) f m = case m of
  Just x  -> Just (f x)
  Nothing -> Nothing

infixl 4 <*>
(<*>) : Maybe (a -> b) -> Maybe a -> Maybe b
(<*>) mf ma = case mf of
  Just f  -> f <$> ma
  Nothing -> Nothing

toDeBruijn : Int -> Dict Name Int -> Expr -> Maybe DeBruijn
toDeBruijn depth dict e = case e of
  Var n     -> 
    case Dict.get n dict of 
      Just ix -> Just (DVar (depth - ix - 1))
      Nothing -> Just (DVar 99)
  App e1 e2 -> DApp <$> toDeBruijn depth dict e1 <*> toDeBruijn depth dict e2
  Lam n  ex -> DLam <$> toDeBruijn (depth + 1) (Dict.insert n depth dict) ex
  Lit x     -> Just (DLit x)

genName : Int -> Name
genName n =
  let name = fromCode ((n % 26) + toCode 'a') in
  if n < 26 then
    String.fromChar name
  else
    String.cons name (genName (n // 26))

mkNewName : Int -> Name
mkNewName n =
  if n < 26 then
    String.fromChar (fromJust (String.toList "xyzuvwabcdefghijklmnopqrst" !! n))
  else
    genName n

toExpr : List Name -> DeBruijn -> Expr
toExpr ctx e = case e of
  DVar n -> case ctx !! n of
    Just name -> Var name
    Nothing   -> let newName = mkNewName (List.length ctx) in
      Var newName
  DApp a b -> App (toExpr ctx a) (toExpr ctx b)
  DLam x -> let newName = mkNewName (List.length ctx) in
    Lam newName (toExpr (newName::ctx) x)
  DLit l   -> Lit l

shift : Int -> Int -> DeBruijn -> DeBruijn
shift d cutoff term = case term of
  DVar k   -> DVar (if k >= cutoff then k + d else k)
  DLam t   -> DLam (shift d (cutoff+1) t)
  DApp t u -> DApp (shift d cutoff t) (shift d cutoff u)
  DLit l   -> DLit l

substitute : DeBruijn -> Int -> DeBruijn -> DeBruijn
substitute tgt j src = case tgt of
  DVar k   -> if k == j then src else tgt
  DLam t   -> DLam (substitute t (j+1) (shift 1 0 src))
  DApp t u -> DApp (substitute t j src) (substitute u j src)
  DLit l   -> DLit l

deBruijnBeta : DeBruijn -> DeBruijn
deBruijnBeta e = case e of
  DApp (DLam body) ex -> shift (-1) 0 (substitute body 0 (shift 1 0 ex))
  DApp a b  -> DApp (deBruijnBeta a) (deBruijnBeta b)
  DLam body -> DLam (deBruijnBeta body)
  x -> x

equivalent : DeBruijn -> DeBruijn -> Bool
equivalent x y = case (x, y) of
  (DLit (LInt l),  DLit (LInt r))  -> l == r
  (DLit (LBool l), DLit (LBool r)) -> l == r
  (DLam ex1,       DLam ex2)       -> equivalent ex1 ex2
  (DVar a,         DVar b)         -> a == b
  (DApp a x,       DApp b y)       -> equivalent a b && equivalent x y
  (_,              _)              -> False
