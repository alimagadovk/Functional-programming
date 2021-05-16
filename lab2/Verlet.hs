{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Verlet where

class Point t where
    infixl 6 .+
    (.+) :: t -> t -> t
    
    infixl 6 .-
    (.-) :: t -> t -> t
    (.-) x y = x .+ ((-1.0) .* y)
    
    infixl 7 .*
    (.*) :: Double -> t -> t
    

instance Point Double where
    x .+ y = x + y
    x .* y = x * y

instance Point (Double, Double) where
    (x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)
    a .* (x, y) = (a * x, a * y)

instance Point [Double] where
    xs .+ ys = zipWith (+) xs ys
    x .* ys = map (x*) ys


verlet :: Point t => (t -> t) -> t -> t -> Double -> [(t,t)]
verlet f x0 v0 dt =
    let x1 = x0 .+ dt .* v0 .+ (0.5 * dt * dt) .* (f x0)
        v1 = v0 .+ (0.5 * dt) .* ((f x0) .+ (f x1))
        verlet' xCurr vCurr =
                let xNext = xCurr .+ dt .* vCurr .+ (0.5 * dt * dt) .* (f xCurr)
                    vNext = vCurr .+ (0.5 * dt) .* ((f xCurr) .+ (f xNext))
                in  (xCurr,vCurr) : verlet' xNext vNext
    in  (x0,v0) : verlet' x1 v1