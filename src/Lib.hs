module Lib
    ( someFunc
    ) where

import Data.Vec3
import Text.Pretty.Simple (pPrint, pShow)

lowerLeft = CVec3 (-0.5) (-0.5) 1.0
xUnit xs = CVec3 (1.0/(fromIntegral xs)) 0 0
yUnit ys = CVec3 0 (1.0/(fromIntegral ys)) 0

rayAt xs ys x y = lowerLeft <+> ((xUnit xs) .^ (fromIntegral x)) <+> ((yUnit ys) .^ (fromIntegral y))

raysAt :: Int -> Int -> Int -> Int -> [(Int, Int, CVec3)]
raysAt xs ys x y
    | x >= xs && y >= ys = []
    | x >= xs && y < ys = raysAt xs ys 0 (y+1)
    | otherwise =  (x, y, rayAt xs ys x y) : raysAt xs ys (x+1) y

viewRays :: Int -> Int -> [(Int, Int, CVec3)]
viewRays xs ys = raysAt xs ys 0 0

someFunc :: IO ()
someFunc = pPrint $ viewRays (4 :: Int) (4 :: Int)
