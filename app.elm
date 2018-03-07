module Main exposing (main)

import Char exposing (isUpper, isLower)
import Debug
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, input, span, text, textarea)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing ((!!), foldl1)
import Parser exposing (Parser, (|.), (|=), ignore, int, keep, keyword, lazy, map,
  oneOf, oneOrMore, repeat, run, succeed, symbol, zeroOrMore)
import Set exposing (Set)

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

mkNewName : List Name -> Name
mkNewName xs = case List.length xs of
  0 -> "x"
  1 -> "y"
  2 -> "z"
  3 -> "a"
  4 -> "b"
  5 -> "c"
  x -> "AAAAA"

toExpr : List Name -> DeBruijn -> Expr
toExpr ctx e = case e of
  DVar n -> case ctx !! n of
    Just name -> Var name
    Nothing   -> let newName = mkNewName ctx in
      Var newName
  DApp a b -> App (toExpr ctx a) (toExpr ctx b)
  DLam x -> let newName = mkNewName ctx in
    Lam newName (toExpr (newName::ctx) x)
  DLit l   -> Lit l

spaces : Parser ()
spaces =
  ignore zeroOrMore (\char -> char == ' ')

i : DeBruijn
i = DLam (DVar 0)

k : DeBruijn
k = DLam (DLam (DVar 1))

s : DeBruijn
s = DLam (DLam (DLam
  (DApp (DApp (DVar 2) (DVar 0))
        (DApp (DVar 1) (DVar 0)))))

s1 = toExpr [] s

o : DeBruijn
o = DLam (DApp (DVar 0) (DVar 0))

y : DeBruijn
y = DLam (DApp (DLam (DApp (DVar 1) (DApp (DVar 0) (DVar 0))))
               (DLam (DApp (DVar 1) (DApp (DVar 0) (DVar 0)))))

y1 : Expr
y1 = Lam "x" (App (Lam "y" (App (Var "x") (App (Var "y") (Var "y"))))
                  (Lam "y" (App (Var "x") (App (Var "y") (Var "y")))))

x : DeBruijn
x = DLam (DApp o (DLam (DApp (DVar 1) (DApp (DVar 0) (DVar 0)))))

iota : DeBruijn
iota = DLam (DApp (DApp (DVar 0) s) k)

type Lit =
    LInt Int
  | LBool Bool

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

yc : Parser Expr
yc =
  succeed y1
    |. keyword "Y"

sc = succeed s1 |. keyword "S"

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

{-
prim : Parser DeBruijn
prim =
  succeed identity
    |= oneOf [scomb, kcomb, icomb]
    |. spaces
-}

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
  [ parens (lazy (\_ -> expr))
  , lit
  , var
  , lazy (\_ -> lam)
  ] |. spaces

name : Parser Name
name =
  keep oneOrMore (\x -> Char.isUpper x || Char.isLower x)

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

expr : Parser Expr
expr =
  succeed (fromJust << foldl1 App)
    |= repeat oneOrMore term

lam : Parser Expr
lam =
  succeed Lam
    |. oneOf [symbol "\\", symbol "λ"]
    |. spaces
    |= name
    |. spaces
    |. oneOf [symbol ".", symbol "->"]
    |. spaces
    |= expr
    |. spaces

showExpr : Expr -> String
showExpr ex = case ex of
  Var name         -> name
  App ex (App a b) -> showExpr ex ++ " (" ++ showExpr a ++ " " ++ showExpr b ++ ")"
  App ex1 ex2      -> showExpr ex1 ++ " " ++ showExpr ex2
  Lam name ex      -> "(λ" ++ name ++ " . " ++ showExpr ex ++ ")"
  Lit (LInt n)     -> toString n
  Lit (LBool b)    -> toString b

showDeBruijn : DeBruijn -> String
showDeBruijn ex = case ex of
  DVar ix           -> toString ix
  DApp e (DApp a b) -> showDeBruijn e ++ " (" ++ showDeBruijn (DApp a b) ++ ")"
  DApp a b          -> showDeBruijn a ++ " " ++ showDeBruijn b
  DLam e            -> "(λ " ++ showDeBruijn e ++ ")"
  DLit (LInt n)     -> toString n
  DLit (LBool b)    -> toString b

asColor : Int -> String
asColor n = case colors !! (n % 6) of
  Just s  -> s
  Nothing -> "rgb(0,0,0)"

colors : List String
colors =
  [ "rgb(127, 127, 255)"
  , "rgb(127, 255, 255)"
  , "rgb(127, 255, 127)"
  , "rgb(255, 255, 127)"
  , "rgb(255, 127, 127)"
  , "rgb(255, 127, 255)"
  ]

styleColorLv : Int -> Attribute msg
styleColorLv l = style [("color", asColor l)]

renderExpr : Int -> Expr -> Html msg
renderExpr level ex = case ex of
  Var name         -> text name
  App e (App a b)  -> span [styleColorLv level]
    [ renderExpr level e
    , text " ("
    , renderExpr (level+1) a
    , text " "
    , renderExpr (level+1) b
    , text ")"
    ]
  App a b -> span [styleColorLv level]
    [ renderExpr level a
    , text " "
    , renderExpr level b
    ]
  Lam n x -> span [styleColorLv level]
    [ text "(λ"
    , text n
    , text " . "
    , renderExpr (level+1) x
    , text ")"
    ]
  Lit (LInt  n) -> text (toString n)
  Lit (LBool b) -> text (toString b)

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

type Step a = Initial a | Intermediate a | Finished String a

evalDB : Int -> DeBruijn -> List (Step DeBruijn)
evalDB n ex =
  let newEx = deBruijnBeta ex in
  if equivalent ex newEx then
    [Finished "Reached normal form." ex]
  else if n > 20 then
    [Finished "Ran out of time." ex]
  else if n == 0 then
    Initial ex :: evalDB (n+1) newEx
  else
    Intermediate ex :: evalDB (n+1) newEx

type alias Model = {
    history : List (Step DeBruijn),
    userInput : String
}

type Msg = Submit | Input String

initModel : Model
initModel = { userInput = "", history = [] }

mkLine : Step DeBruijn -> Html msg
mkLine ex = 
    let (e, prefix) = case ex of
      Intermediate e -> (e, " ... ")
      Initial e -> (e, "")
      Finished reason e -> (e, reason ++ " | ")
    in div []
    [ text prefix
    , renderExpr 0 (toExpr [] e)
    ]

view : Model -> Html Msg
view model =
  div [style [("background-color", "black"), ("color", "white")]]
    [ div [] (List.map mkLine model.history)
    , input [onInput Input] []
    , button [onClick Submit] [text "Submit"]
    ]

parseAppend : Parser Expr -> String -> List Expr -> List Expr
parseAppend p str hist = case run p str of
  Ok ex -> ex :: hist
  Err _ -> hist

parseEval : Parser Expr -> String -> List (Step DeBruijn)
parseEval p str = case run p str of
  Ok ex -> (case toDeBruijn 0 Dict.empty ex of
             Just e  -> evalDB 0 e
             Nothing -> [Initial (DVar 666)])
  Err _ -> []

update : Msg -> Model -> Model
update msg model = case msg of
  Submit  -> { model | history = parseEval expr (model.userInput),
                       userInput = "" }
  Input s -> { model | userInput = s }

main = Html.beginnerProgram
  { model  = initModel
  , view   = view
  , update = update
  }
