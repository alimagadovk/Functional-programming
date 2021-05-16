{-# LANGUAGE OverloadedStrings #-}

import EasyTest
import SimpleLang

allTests = tests
    [ scope "state" $ tests
        [ expect $ empty "x" == 0
        , expect $ (let state = extend empty "x" 50 in (state "x")) == 50
        ]
    , scope "eval" $ tests
        [ expect $ eval empty (Val 5) == 5
        , expect $ eval (extend empty "a" 1) (Op (Val 1) Eql (Var "a")) == 1
        ]
    , scope "desugar" $ tests
        [ expect $ desugar (Incr "A") == DAssign "A" (Op (Var "A") Plus (Val 1))
        ]
    , scope "run" $ tests
        [ expect $ runSimpler empty (DAssign "A" (Val 10)) "A" == 10
        , expect $ SimpleLang.run empty (Incr "A") "A" == 1
        , expect $ ((SimpleLang.run (extend empty "In" 4) factorial) "Out") == 24
        , expect $ ((SimpleLang.run (extend empty "In" 4) fibonacci) "Out") == 5
        , expect $ ((SimpleLang.run (extend empty "A" 100) squareRoot) "B") == 10
        ]
    ]

main = EasyTest.run allTests