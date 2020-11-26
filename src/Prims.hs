{-# LANGUAGE OverloadedStrings #-}

module Prims where
    
import Language
import Prelude hiding (null, head, tail, filter, foldl, foldr, map, or, and, not, pred)
import Data.Map (fromList, Map)

lams :: [Name] -> UntypedExpr -> UntypedExpr
lams [] e = e
lams (x:xs) e = ULam x (lams xs e)

-- arithmetic
zero,one,two,three,s,add :: UntypedExpr
zero = ULam "f" (ULam "x" "x")
one = ULam "f" (ULam "x" ("f" ## "x"))
two = ULam "f" (ULam "x" ("f" ## ("f" ## "x")))
three = lams ["f","x"] ("f" ## ("f" ## ("f" ## "x")))
s = lams ["m", "s", "z"] ("s" ## ("m" ## "s" ## "z"))
add = lams ["m","n"] ("m" ## s ## "n")
pred = ULam "n" (pi2 ## ("n" ## phi ## (pair ## zero ## zero)))
    where pi1 = head
          pi2 = tail
          pair = cons
          phi = ULam "p" (pair ## (s ## (pi1 ## "p")
                              ## (pi1 ## "p")))
sub = lams ["m", "n"] ("n" ## pred ## "m")
isZero = ULam "n" ("n" ## (ULam "x" f) ## t)
eqNat = lams ["m", "n"] (and ## (isZero ## (sub ## "m" ## "n"))
                             ## (isZero ## (sub ## "n" ## "m")))

fromInt :: Int -> UntypedExpr
fromInt = lams ["f","x"] . go
    where go :: Int -> UntypedExpr
          go 0 = "x"
          go n = "f" ## (go (n - 1))

-- logic
t,f,and,or,not :: UntypedExpr
t = lams ["t","f"] "t"
f = lams ["t","f"] "f"
and = lams ["p","q"] ("p" ## "q" ## f)
or = lams ["p","q"] ("p" ## t ## "q")
not = ULam "p" ("p" ## f ## t)

-- recursion
y :: UntypedExpr
y = ULam "g" ((ULam "x" ("g" ## ("x" ## "x"))) ## 
             (ULam "x" ("g" ## ("x" ## "x"))))

-- lists
cons, nil, null, head, tail :: UntypedExpr
cons = lams ["h","t","f"] ("f" ## "h" ## "t")
nil = ULam "x" t
null = ULam "l" ("l" ## lams ["h","t"] f)
head = ULam "l" ("l" ## lams ["h","t"] "h")
tail = ULam "l" ("l" ## lams ["h","t"] "t")

map,filter, foldr, foldl :: UntypedExpr
map = y ## lams ["F", "f", "l"] (
    (null ## "l") ## nil
                 ## (cons ## ("f" ## (head ## "l"))
                         ## ("F" ## "f" ## (tail ## "l"))))
filter = 
    y ## (lams ["filter","p","l"]
            ((null ## "l") ##
                nil ##
                (("p" ## (head ## "l")) ##
                    (cons ## (head ## "l") ##
                        ("filter" ## "p" ## (tail ## "l"))) ##
                    ("filter" ## "p" ## (tail ## "l")))))
foldr =
    y ## (lams ["F", "g", "lst", "zero"]
            ((null ## "lst") ##
                "zero" ##
                ("g" ## (head ## "lst")
                     ## ("F" ## "g" 
                            ## (tail ## "lst")
                            ## "zero"))))
foldl = 
    y ## lams ["F", "g", "zero", "lst"]
            ((null ## "lst") ## 
                "zero" ##
                ("F" ## "g" 
                     ## ("g" ## "zero" ## (head ## "lst"))) 
                     ## (tail ## "lst"))

builtins :: Map Name (UntypedExpr)
builtins = fromList 
    [ ("s", s)
    , ("add", add)
    , ("sub", sub)
    , ("pred", pred)
    , ("isZero", isZero)
    , ("eqNat", eqNat)
    , ("cons", cons)
    , ("filter", filter)
    , ("foldl", foldl)
    , ("foldr", foldr)
    , ("head", head)
    , ("tail", tail)
    , ("map", map)
    , ("nil", nil)
    , ("null", null)
    , ("TRUE", t)
    , ("FALSE", f)
    , ("or", or)
    , ("and", and)
    , ("not", not)
    , ("y", y)
    ]