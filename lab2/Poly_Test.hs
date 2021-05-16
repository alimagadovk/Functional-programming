{-# LANGUAGE OverloadedStrings #-}

import EasyTest
import Poly

allTests = tests
    [ scope "applyPoly" $ tests [
        expect $ let
                    x = P [1, 2, 1]
        in applyPoly x 0 == 1
        ],
        scope "(==)" $ tests [
        expect $ let
                    x = P [1, 2, 1]
                    y = P [1, 2, 1, 0, 0]
        in (x == y) == True,
        expect $ let
                    x = P [1, 2, 2]
                    y = P [1, 2, 1, 0, 0]
        in (x == y) == False
        ],
        scope "show" $ tests [
        expect $ let
                    x = P [1, 2, 1]
        in show x == "x^2 + 2 * x + 1"
        ],
        scope "plus" $ tests [
        expect $ let
                    x = P [1, 2, 1]
                    y = P [2, -2, 3]
        in plus x y == P [3, 0, 4]
        ],
        scope "times" $ tests [
        expect $ let
                    x = P [1, 2, 1]
                    y = P [2, -2, 3]
        in times x y == P [2, 2, 1, 4, 3]
        ],
        scope "nderiv" $ tests [
        expect $ let
                    x = P [1, 2, 1]
                    y = P [2, -2, 3]
        in nderiv 3 (times x y) == P [24, 72]
        ]
    ]

main = EasyTest.run allTests