module Common exposing (fromJust, last)

import Debug

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

last : List a -> a
last xs = case xs of
  []       -> Debug.crash "rip"
  x :: []  -> x
  x :: s   -> last s
