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

-- \x . (\y . (\z . x z (y z)))    map []                  depth 0
-- \. (\y . (\z . x z (y z)))      map [(x:0)]             depth 1
-- \. (\. (\z. x z (y z)))         map [(x:0),(y:1)]       depth 2
-- \. (\. (\. x z (y z)))          map [(x:0),(y:1),(z:2)] depth 3
-- \. (\. (\. 3 1 (2 1)))          replace each var with depth - map(var)

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
      Just ix -> Just (DVar (depth - ix))
      Nothing -> Just (DVar 99)
  App e1 e2 -> DApp <$> toDeBruijn depth dict e1 <*> toDeBruijn depth dict e2
  Lam n  ex -> DLam <$> toDeBruijn (depth + 1) (Dict.insert n depth dict) ex
  Lit x     -> Just (DLit x)

spaces : Parser ()
spaces =
  ignore zeroOrMore (\char -> char == ' ')

i : DeBruijn
i = DLam (DVar 1)

k : DeBruijn
k = DLam (DLam (DVar 2))

s : DeBruijn
s = DLam (DLam (DLam
  (DApp (DApp (DVar 3) (DVar 1))
        (DApp (DVar 2) (DVar 1)))))

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

{-#
prim : Parser DeBruijn
prim =
  succeed identity
    |= oneOf [scomb, kcomb, icomb]
    |. spaces
#-}

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
  DApp e (DApp a b) -> showDeBruijn e ++ " (" ++ showDeBruijn a ++ " " ++ showDeBruijn b ++ ")"
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

showSet : Set Name -> String
showSet s = Set.foldr (\a b -> a ++ " " ++ b) "" s

freeVars : Expr -> Set Name
freeVars e = case e of
  Lit _   -> Set.empty
  Var n   -> Set.singleton n
  App a b -> Set.union (freeVars a) (freeVars b)
  Lam n x -> Set.diff  (freeVars x) (Set.singleton n)

betaReduce : Expr -> Expr
betaReduce ex = case ex of
  (App (Lam var body) ex1) -> substSafe var ex1 body
  (App x y)                -> App (betaReduce x) (betaReduce y)
  (Lam x (App y z))        -> Lam x (betaReduce (App y z))
  x                        -> x

promoteFreeVars : Int -> Int -> DeBruijn -> DeBruijn
promoteFreeVars amt depth ex = case ex of
  DVar n   -> if n > depth then DVar (n + amt) else DVar n
  DLam e   -> DLam (promoteFreeVars amt (depth+1) e)
  DApp a b -> DApp (promoteFreeVars amt depth a) (promoteFreeVars amt depth b)
  x        -> x

replace : Int -> DeBruijn -> DeBruijn -> DeBruijn
replace depth body new = case body of
  DLit l   -> DLit l
  DLam ex  -> DLam (replace (depth+1) ex new)
  DApp a b -> DApp (replace depth a new) (replace depth b new)
  DVar n   ->
    if n == depth then
      promoteFreeVars depth 0 new
    else if n > depth then
      DVar (n - 1)
    else
      DVar n

deBruijnBeta : Int -> DeBruijn -> DeBruijn
deBruijnBeta depth e = case e of
  DApp (DLam body) ex -> replace depth body ex
  DApp a b -> DApp (deBruijnBeta depth a) (deBruijnBeta depth b)
  DLam body -> DLam (deBruijnBeta (depth+1) body)
  x -> x

{-#
SS              =  (λx . (λy . (λz . x z (y z))))   (λx . (λy . (λz . x z (y z))))
  {beta reduce} -> (λy . (λz . (λx1 . (λy . (λz . x1 z (y z)))) z (y z)))
                     ^     ^            ^     ^
                                        |_____|_ These need to be renamed when substituted
#-}

-- Generate an alpha-equivalent expression that contains none of the names in
-- the input set
makeUnique : Set Name -> Expr -> Expr
makeUnique s e = case e of
  App a b -> App (makeUnique s a) (makeUnique s b)
  Lam n x ->
    if Set.member n s then
      Lam (n ++ "1") (makeUnique s x)
    else
      Lam n (makeUnique s x)
  Var x   ->
    if Set.member x s then
      Var (x ++ "1")
    else
      Var x
  x       -> x

substSafe : Name -> Expr -> Expr -> Expr
substSafe var new e =
  let fv = freeVars e in
  if Set.isEmpty fv then
    subst var new e
  else
    subst var (makeUnique fv new) e

-- FIXME: perform renaming to avoid variable capture
subst : Name -> Expr -> Expr -> Expr
subst var new e = case e of
  Var name    -> if name == var then new else Var name
  App ex1 ex2 -> App (subst var new ex1) (subst var new ex2)
  Lam name ex -> Lam name (subst var new ex)
  Lit l       -> Lit l

equivalent : Expr -> Expr -> Bool
equivalent x y = case (x, y) of
  (Lit (LInt l),  Lit (LInt r))  -> l == r
  (Lit (LBool l), Lit (LBool r)) -> l == r
  (Lam v1 ex1,    Lam v2 ex2)    -> v1 == v2 && equivalent ex1 ex2
  (Var a,         Var b)         -> a == b
  (App a x,       App b y)       -> equivalent a b && equivalent x y
  (_,             _)             -> False

-- FIXME: doesn't catch cycles with period > 1
eval : Int -> Expr -> List Expr
eval n ex =
  let newEx = betaReduce ex in
  if n > 20 then
    [ex]
  else
    ex :: eval (n+1) newEx

evalDB : Int -> DeBruijn -> List DeBruijn
evalDB n ex =
  let newEx = deBruijnBeta 0 ex in
  if n > 20 then
    [ex]
  else
    ex :: evalDB (n+1) newEx

type alias Model = {
    history : List DeBruijn,
    userInput : String
}

type Msg = Submit | Input String

initModel : Model
initModel = { userInput = "", history = [] }

mkLine : DeBruijn -> Html msg
mkLine ex = div []
    [ text (showDeBruijn ex)
    ]

view : Model -> Html Msg
view model =
  div [style [("background-color", "black")]]
    [ div [] (List.map mkLine model.history)
    , input [onInput Input] []
    , button [onClick Submit] [text "Submit"]
    ]

parseAppend : Parser Expr -> String -> List Expr -> List Expr
parseAppend p str hist = case run p str of
  Ok ex -> ex :: hist
  Err _ -> hist

parseEval : Parser Expr -> String -> List DeBruijn
parseEval p str = case run p str of
  Ok ex -> (case toDeBruijn 0 Dict.empty ex of
             Just e  -> evalDB 0 e
             Nothing -> [])
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
