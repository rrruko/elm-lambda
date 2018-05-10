module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, style, id, type_, defaultValue)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing ((!!))

import Common exposing (last)
import Lambda exposing (..)
import ParseLambda exposing (..)
import Eval exposing (..)

asColor : Int -> String
asColor n = case colors !! (n % 6) of
  Just s  -> s
  Nothing -> mkCssRgb (mkRgb 0 0 0)

clamp : Int -> Int -> Int -> Int
clamp lo hi t = min hi (max lo t)

type Rgb = Rgb Int Int Int

mkRgb : Int -> Int -> Int -> Rgb
mkRgb r g b =
  Rgb
    (clamp 0 255 r)
    (clamp 0 255 g)
    (clamp 0 255 b)

mkCssRgb : Rgb -> String
mkCssRgb rgb = case rgb of
  Rgb r g b ->
    "rgb(" ++ toString r ++
      ", " ++ toString g ++
      ", " ++ toString b ++
      ")"

colors : List String
colors =
  [ mkCssRgb (mkRgb 127 127 255)
  , mkCssRgb (mkRgb 127 255 255)
  , mkCssRgb (mkRgb 127 255 127)
  , mkCssRgb (mkRgb 255 255 127)
  , mkCssRgb (mkRgb 255 127 127)
  , mkCssRgb (mkRgb 255 127 255)
  ]

styleColorLv : Int -> Attribute msg
styleColorLv l = style [("color", asColor l)]

renderExpr : Int -> Int -> Expr -> Html msg
renderExpr maxDepth level ex =
  if level < maxDepth then
    case ex of
      Var name         -> text name
      App e (App a b)  -> span [styleColorLv level]
        [ renderExpr maxDepth level e
        , text " ("
        , renderExpr maxDepth (level+1) (App a b)
        , text ")"
        ]
      App a b -> span [styleColorLv level]
        [ renderExpr maxDepth level a
        , text " "
        , renderExpr maxDepth level b
        ]
      Lam n x -> span [styleColorLv level]
        [ text "(λ"
        , text n
        , text " . "
        , renderExpr maxDepth (level+1) x
        , text ")"
        ]
      Lit (LInt  n) -> text (toString n)
      Lit (LBool b) -> text (toString b)
  else
    text "..."

renderDeBruijn : Int -> Int -> DeBruijn -> Html msg
renderDeBruijn maxDepth level ex =
  if level < maxDepth then
    case ex of
      DVar n             -> text (toString n)
      DApp e (DApp a b)  -> span [styleColorLv level]
        [ renderDeBruijn maxDepth level e
        , text " ("
        , renderDeBruijn maxDepth (level+1) (DApp a b)
        , text ")"
        ]
      DApp a b -> span [styleColorLv level]
        [ renderDeBruijn maxDepth level a
        , text " "
        , renderDeBruijn maxDepth level b
        ]
      DLam x -> span [styleColorLv level]
        [ text "(λ "
        , renderDeBruijn maxDepth (level+1) x
        , text ")"
        ]
      DLit (LInt  n) -> text (toString n)
      DLit (LBool b) -> text (toString b)
  else
    text "..."

type alias Model = {
    history      : List (Step DeBruijn),
    userInput    : String,
    showDeBruijn : Bool,
    maxSteps     : Int,
    maxDepth     : Int
}

type Msg =
    Submit
  | Input String
  | ToggleShowDeBruijn
  | SetMaxSteps Int
  | SetMaxDepth Int
  | ShowHelp

initModel : Model
initModel = {
  history      = [],
  userInput    = "",
  showDeBruijn = False,
  maxSteps     = 200,
  maxDepth     = 15
  }

mkLine : Step DeBruijn -> Model -> Html msg
mkLine ex model =
  let exprSpan =
        if model.showDeBruijn then
          renderDeBruijn model.maxDepth 0 (unstep ex)
        else
          renderExpr model.maxDepth 0 (toExpr [] (unstep ex))
  in  case ex of
        Intermediate e -> div [] [indent exprSpan]
        Initial e -> div [] [exprSpan]
        Finished reason e ->
          div [] [indent (span []
            [ exprSpan
            , text " | "
            , text reason
            ])]

indent : Html msg -> Html msg
indent html = span [] [text "-> ", html]

welcome : List (Html msg)
welcome =
  [ p []
      [ text "This is a lambda calculus interpreter. "
      , text "It takes lambda calculus expressions and beta-reduces them. "
      , text "It understands the S, K, I, and Y combinators, and iota. "
      , text "Try one of these expressions to see how it works: "
      ]
  , ul []
      [ li [] [text "(\\x. \\y. x)"]
      , li [] [text "(\\f. (\\x. f (x x)) (\\x. f (x x)))"]
      , li [] [text "(\\f. f (\\x. \\y. \\z. x z (y z)) (\\x. \\y. x)) (\\f. f (\\x. \\y. \\z. x z (y z)) (\\x. \\y. x))"]
      , li [] [text "iota iota"]
      , li [] [text "S K K"]
      ]
  ]

view : Model -> Html Msg
view model =
  div []
    [ div [class "output"]
        (if List.isEmpty model.history then
          welcome
        else
          List.map (\db -> mkLine db model) model.history)
    , div [id "form"]
        [ input [onInput Input] []
        , button [onClick Submit] [text "Submit"]
        , button [onClick ShowHelp] [text "Show help"]
        ]
    , div [id "controls"]
        [ input [type_ "checkbox", onClick ToggleShowDeBruijn] []
        , text "Show expressions in De Bruijn index notation"
        , br [] []
        , input
          [type_ "number"
          , defaultValue (toString model.maxDepth)
          , onInput (SetMaxDepth << Result.withDefault 0 << String.toInt)] []
        , text "Maximum depth at which to show expressions"
        , br [] []
        , input
          [type_ "number"
          , defaultValue (toString model.maxSteps)
          , onInput (SetMaxSteps << Result.withDefault 0 << String.toInt)] []
        , text "Maximum evaluation steps allowed"
        ]
    ]

update : Msg -> Model -> Model
update msg model = case msg of
  Submit             -> { model | history = parseEval expr model.userInput model.maxSteps }
  Input s            -> { model | userInput = s }
  ToggleShowDeBruijn -> { model | showDeBruijn = not model.showDeBruijn }
  SetMaxDepth n      -> { model | maxDepth = n }
  SetMaxSteps n      -> { model | maxSteps = n }
  ShowHelp           -> { model | history = [] }

main : Program Never Model Msg
main = Html.beginnerProgram
  { model  = initModel
  , view   = view
  , update = update
  }
