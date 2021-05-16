{-# LANGUAGE OverloadedStrings #-} 
import EasyTest
import Verlet


allTests = tests [
    scope "1D" $ tests [
        expect $ let 
                    acceleration :: Double -> Double
                    acceleration x = x
                    y = verlet acceleration 0.0 0.5 2
        in (take 5 y == [(0.0,0.5),(1.0,1.5),(6.0,8.5),(35.0,49.5),(204.0,288.5)])
    ],
    scope "2D" $ tests [
        expect $ let 
                    acceleration :: (Double,Double) -> (Double,Double)
                    acceleration _ = (0.0, 0.0)
                    y = verlet acceleration (0.0, 0.0) (0.1, 0.2) 1.0
        in (take 5 y == [((0.0,0.0),(0.1,0.2)),((0.1,0.2),(0.1,0.2)),((0.2,0.4),(0.1,0.2)),((0.30000000000000004,0.6000000000000001),(0.1,0.2)),((0.4,0.8),(0.1,0.2))])
    ],
    scope "3D" $ tests [
        expect $ let 
                    acceleration :: [Double] -> [Double]
                    acceleration xs = [0*x | x <- xs]
                    y = verlet acceleration [0.0, 0.0, 0.0] [0.1, 0.1, 0.1] 0.5
        in (take 5 y == [([0.0,0.0,0.0],[0.1,0.1,0.1]),([5.0e-2,5.0e-2,5.0e-2],[0.1,0.1,0.1]),([0.1,0.1,0.1],[0.1,0.1,0.1]),([0.15000000000000002,0.15000000000000002,0.15000000000000002],[0.1,0.1,0.1]),([0.2,0.2,0.2],[0.1,0.1,0.1])])
    ]
 ]

main = EasyTest.run allTests