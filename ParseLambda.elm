module ParseLambda exposing (expr)

import Char
import Dict
import List.Extra exposing (foldl1)
import Parser exposing (Parser, (|.), (|=), ignore, int, keep, keyword, lazy, map,
  oneOf, oneOrMore, repeat, run, succeed, symbol, zeroOrMore)

import Common exposing (fromJust)
import Lambda exposing (Expr(..), DeBruijn(..), Name, Lit(..), toExpr, toDeBruijn)
import Literals exposing (..)

app : List Expr -> Expr
app = fromJust << foldl1 App

spaces : Parser ()
spaces =
  ignore zeroOrMore (\char -> char == ' ')

scomb : Parser DeBruijn
scomb =
  succeed s
    |. keyword "S"

kcomb : Parser DeBruijn
kcomb =
  succeed k
    |. keyword "K"

icomb : Parser DeBruijn
icomb =
  succeed i
    |. keyword "I"

ycomb : Parser DeBruijn
ycomb = 
  succeed y
    |. keyword "Y"

parsepred : Parser DeBruijn
parsepred =
  succeed (fromJust <| toDeBruijn 0 Dict.empty pred)
    |. keyword "pred"

parseiszero : Parser DeBruijn
parseiszero = 
  succeed (fromJust <| toDeBruijn 0 Dict.empty iszero)
    |. keyword "iszero"

parsetimes : Parser DeBruijn
parsetimes =
  succeed (fromJust <| toDeBruijn 0 Dict.empty times)
    |. keyword "times"

parseiota : Parser DeBruijn
parseiota =
  succeed iota
    |. keyword "iota"

ltrue : Parser Lit
ltrue =
  succeed (LBool True)
    |. keyword "true"

lfalse : Parser Lit
lfalse =
  succeed (LBool False)
    |. keyword "false"

lint : Parser Lit
lint =
  succeed LInt
    |= int

prim : Parser Expr
prim =
  succeed (\db -> toExpr [] db)
    |= oneOf [scomb, kcomb, icomb, ycomb, parsepred, parseiszero, parsetimes,
           parseiota]
    |. spaces

lit : Parser Expr
lit =
  succeed Lit
    |= oneOf [ltrue, lfalse, lint]
    |. spaces

var : Parser Expr
var =
  succeed Var
    |= name
    |. spaces

parens : Parser a -> Parser a
parens p =
  succeed identity
    |. symbol "("
    |= p
    |. symbol ")"
    |. spaces


term : Parser Expr
term = oneOf
  [ prim
  , lit
  , var
  , parens (lazy (\_ -> expr))
  , lazy (\_ -> lam)
  ] |. spaces


name : Parser Name
name =
  keep oneOrMore (\x -> Char.isUpper x || Char.isLower x)

expr : Parser Expr
expr =
  succeed app
    |= repeat oneOrMore (lazy (\_ -> term))

lam : Parser Expr
lam =
  succeed Lam
    |. oneOf [symbol "\\", symbol "λ"]
    |. spaces
    |= name
    |. spaces
    |. oneOf [symbol ".", symbol "->"]
    |. spaces
    |= lazy (\_ -> expr)
    |. spaces
