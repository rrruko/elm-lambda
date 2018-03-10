module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing ((!!))

import Lambda exposing (..)
import ParseLambda exposing (..)
import Eval exposing (..)

showExpr : Expr -> String
showExpr ex = case ex of
  Var name         -> name
  App ex (App a b) -> showExpr ex ++ " (" ++ showExpr (App a b) ++ ")"
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
    , renderExpr (level+1) (App a b)
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

welcome : List (Html msg)
welcome =
  [ p [] 
      [ text "This is a lambda calculus interpreter. "
      , text "It takes lambda calculus expressions and beta-reduces them. "
      , text "It understands the S, K, I, and Y combinators, and iota. "
      ]
  , ul []
      [ li [] [text "(\\x. \\y. x)"]
      , li [] [text "(\\f. (\\x. f (x x)) (\\x. f (x x)))"]
      , li [] [text "(\\f. f (\\x. \\y. \\z. x z (y z)) (\\x. \\y. x)) (\\f. f (\\x. \\y. \\z. x z (y z)) (\\x. \\y. x))"]
      , li [] [text "iota iota"]
      , li [] [text "S K K"]
      ]
  ]

last : List a -> a
last xs = case xs of
  []       -> Debug.crash "rip"
  x :: []  -> x
  x :: s   -> last s

view : Model -> Html Msg
view model =
  div []
    [ div [class "output"]
        (if List.isEmpty model.history then
          welcome
        else
          [mkLine (last model.history)])
    , input [onInput Input] []
    , button [onClick Submit] [text "Submit"]
    ]

update : Msg -> Model -> Model
update msg model = case msg of
  Submit  -> { model | history = parseEval expr (model.userInput),
                       userInput = "" }
  Input s -> { model | userInput = s }

{-
type alias Model = Int
type alias Msg = Never
initModel = 0
view m = text "it works"
update msg m = m
-}

main : Program Never Model Msg
main = Html.beginnerProgram
  { model  = initModel
  , view   = view
  , update = update
  }
