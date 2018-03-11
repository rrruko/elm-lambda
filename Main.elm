module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, style, id, type_)
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

renderDeBruijn : Int -> DeBruijn -> Html msg
renderDeBruijn level ex = case ex of
  DVar n             -> text (toString n)
  DApp e (DApp a b)  -> span [styleColorLv level]
    [ renderDeBruijn level e
    , text " ("
    , renderDeBruijn (level+1) (DApp a b)
    , text ")"
    ]
  DApp a b -> span [styleColorLv level]
    [ renderDeBruijn level a
    , text " "
    , renderDeBruijn level b
    ]
  DLam x -> span [styleColorLv level]
    [ text "(λ "
    , renderDeBruijn (level+1) x
    , text ")"
    ]
  DLit (LInt  n) -> text (toString n)
  DLit (LBool b) -> text (toString b)

type alias Model = {
    history : List (Step DeBruijn),
    userInput : String,
    showDeBruijn : Bool
}

type Msg =
    Submit
  | Input String
  | ToggleShowDeBruijn
  | ShowHelp

initModel : Model
initModel = { userInput = "", history = [], showDeBruijn = False }

mkLine : Step DeBruijn -> Bool -> Html msg
mkLine ex showDeBruijn =
  let exprSpan =
        if showDeBruijn then
          renderDeBruijn 0 (unstep ex)
        else
          renderExpr 0 (toExpr [] (unstep ex))
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
          List.map (\db -> mkLine db model.showDeBruijn) model.history)
    , div [id "form"]
        [ input [onInput Input] []
        , button [onClick Submit] [text "Submit"]
        , button [onClick ShowHelp] [text "Show help"]
        ]
    , div [id "controls"]
        [ input [type_ "checkbox", onClick ToggleShowDeBruijn] []
        , text "Show expressions in De Bruijn index notation"
        ]
    ]

update : Msg -> Model -> Model
update msg model = case msg of
  Submit             -> { model | history = parseEval expr (model.userInput) }
  Input s            -> { model | userInput = s }
  ToggleShowDeBruijn -> { model | showDeBruijn = not model.showDeBruijn }
  ShowHelp           -> { model | history = [] }

main : Program Never Model Msg
main = Html.beginnerProgram
  { model  = initModel
  , view   = view
  , update = update
  }
