module Main exposing (main)

import Char exposing (isUpper, isLower)
import Debug
import Html exposing (Html, button, div, input, text, textarea)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (foldl1)
import Parser exposing (Parser, (|.), (|=), ignore, int, keep, keyword, lazy, map, 
  oneOf, oneOrMore, repeat, run, succeed, symbol, zeroOrMore)

type alias Name = String

type Expr =
    Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  
spaces : Parser ()
spaces =
  ignore zeroOrMore (\char -> char == ' ')

i : Expr
i = Lam "x" (Var "x")

k : Expr
k = Lam "x" (Lam "y" (Var "x"))

s : Expr
s = Lam "x"
      (Lam "y"
        (Lam "z"
          (App (App (Var "x") (Var "z"))
               (App (Var "y") (Var "z")))))

type Lit =
    LInt Int
  | LBool Bool

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
    ]

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
    |. symbol "\\"
    |. spaces
    |= name
    |. spaces
    |. symbol "."
    |. spaces
    |= expr
    |. spaces
    
showExpr : Expr -> String
showExpr ex = case ex of
  Var name         -> name
  App ex (App a b) -> showExpr ex ++ " (" ++ showExpr a ++ " " ++ showExpr b ++ ")"
  App ex1 ex2      -> showExpr ex1 ++ " " ++ showExpr ex2
  Lam name ex      -> "(\\" ++ name ++ " -> " ++ showExpr ex ++ ")"
  Lit (LInt n)     -> toString n
  Lit (LBool b)    -> toString b

type alias Model = {
    history : List Expr,
    userInput : String
}

type Msg = Submit | Input String

initModel : Model
initModel = { userInput = "", history = [] }

mkLine : Expr -> Html msg
mkLine ex = div [] [text (showExpr ex)]
    
view : Model -> Html Msg
view model =
  div []
    [ div [] (List.map mkLine model.history)
    , input [onInput Input] []
    , button [onClick Submit] [text "Submit"]
    ]
    
parseAppend : Parser Expr -> String -> List Expr -> List Expr
parseAppend p str hist = case run p str of
  Ok ex -> ex :: hist
  Err _ -> hist

update : Msg -> Model -> Model
update msg model = case msg of
  Submit  -> { model | history = parseAppend expr (model.userInput) model.history, 
                       userInput = "" }
  Input s -> { model | userInput = s }

main = Html.beginnerProgram 
  { model  = initModel
  , view   = view
  , update = update
  }
