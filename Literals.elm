module Literals exposing (..)

import Lambda exposing (DeBruijn(..), Expr(..), Lit)

i : DeBruijn
i = DLam (DVar 0)

k : DeBruijn
k = DLam (DLam (DVar 1))

s : DeBruijn
s = DLam (DLam (DLam
  (DApp (DApp (DVar 2) (DVar 0))
        (DApp (DVar 1) (DVar 0)))))

o : DeBruijn
o = DLam (DApp (DVar 0) (DVar 0))

y : DeBruijn
y = DLam (DApp (DLam (DApp (DVar 1) (DApp (DVar 0) (DVar 0))))
               (DLam (DApp (DVar 1) (DApp (DVar 0) (DVar 0)))))

x : DeBruijn
x = DLam (DApp o (DLam (DApp (DVar 1) (DApp (DVar 0) (DVar 0)))))

iota : DeBruijn
iota = DLam (DApp (DApp (DVar 0) s) k)

pred : Expr
pred = 
  let b = Lam "g" (Lam "h" (App (Var "h") (App (Var "g") (Var "f")))) in
  let c = Lam "u" (Var "x") in
  let d = Lam "u" (Var "u") in
  Lam "n" (Lam "f" (Lam "x" (App (App (App (Var "n") b) c) d)))

lamfalse : Expr
lamfalse = Lam "a" (Lam "b" (Var "b"))

lamtrue : Expr
lamtrue = Lam "a" (Lam "b" (Var "a"))

iszero : Expr
iszero = Lam "n" (App (App (Var "n") (Lam "x" lamfalse)) lamtrue)

times : Expr
times = Lam "m" (Lam "n" (Lam "f" (App (Var "m") (App (Var "n") (Var "f")))))

factorial : String
factorial = "Y (\\f. \\n. iszero n (\\g.\\x.g x) (times n (f (pred n)))) (\\g. \\x. g (g (g x)))"


