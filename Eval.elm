module Eval exposing (Step(..), parseEval, unstep)

import Dict exposing (Dict)
import Lambda exposing (DeBruijn(..), Expr(..), deBruijnBeta, equivalent, toDeBruijn)
import Parser exposing (Parser, run)

type Step a = Initial a | Intermediate a | Finished String a

unstep : Step DeBruijn -> DeBruijn
unstep ex = case ex of
  Intermediate e    -> e
  Initial e         -> e
  Finished reason e -> e

evalDB : Int -> Int -> DeBruijn -> List (Step DeBruijn)
evalDB n maxDepth ex =
  let newEx = deBruijnBeta ex in
  if equivalent ex newEx then
    [Finished "Reached normal form." ex]
  else if n >= maxDepth then
    [Finished "Ran out of time." ex]
  else if n == 0 then
    Initial ex :: evalDB (n+1) maxDepth newEx
  else
    Intermediate ex :: evalDB (n+1) maxDepth newEx

parseAppend : Parser Expr -> String -> List Expr -> List Expr
parseAppend p str hist = case run p str of
  Ok ex -> ex :: hist
  Err _ -> hist

parseEval : Parser Expr -> String -> Int -> List (Step DeBruijn)
parseEval p str maxDepth = case run p str of
  Ok ex -> (case toDeBruijn 0 Dict.empty ex of
             Just e  -> evalDB 0 maxDepth e
             Nothing -> [Initial (DVar 666)])
  Err _ -> []
