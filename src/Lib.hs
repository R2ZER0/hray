{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Vec3
import Text.Pretty.Simple (pPrint, pShow)
import qualified SDL as SDL
import qualified Control.Exception as Exception
import Foreign.C.Types (CInt)
import Control.Concurrent (threadDelay)

width :: Int
height :: Int

width = 400
height = width `div` 2

-- Colours
type Colour = CVec3
white = CVec3 1.0 1.0 1.0
red = CVec3 1.0 0.0 0.0
sunsetRed = (CVec3 255 166 158) .^ (1.0 / 255.0)
skyBlue = (CVec3 135 206 235) .^ (1.0 / 255.0)

vec x y z = CVec3 x y z

-- Ray point direction
data Ray = Ray CVec3 CVec3 deriving (Show, Eq)

-- Sphere point radius
data Sphere = Sphere CVec3 Double

lowerLeft = vec (-0.5) (-0.5) 1.0
xUnit xs = vec (1.0/(fromIntegral xs)) 0 0
yUnit ys = vec 0 (1.0/(fromIntegral ys)) 0

cameraLocation = vec 0.0 1.8 10.0   
cameraFov = 45.0
cameraVector = vec 0.0 3.0 0.0

rayAt xs ys x y = Ray cameraLocation (normalize (lowerLeft <+> ((xUnit xs) .^ (fromIntegral x)) <+> ((yUnit ys) .^ (fromIntegral y))))

rayAt' :: Int -> Int -> Int -> Int -> Ray
rayAt' xs ys x y = Ray cameraLocation (normalize (eyeVector <+> yv <+> xv))
    where
        eyeVector = normalize (cameraVector <-> cameraLocation)
        vpRight = normalize (eyeVector >< (vec 0.0 1.0 0.0))
        vpUp = normalize (vpRight >< eyeVector)
        fovRadians = pi * (cameraFov / 2.0) / 180.0
        heightWidthRatio = (fromIntegral ys :: Double) / (fromIntegral xs :: Double)
        halfWidth = tan fovRadians :: Double
        halfHeight = heightWidthRatio * halfWidth :: Double
        cameraWidth = halfWidth * 2
        cameraHeight = halfHeight * 2
        pixelWidth = cameraWidth / ((fromIntegral xs :: Double) - 1)
        pixelHeight = cameraHeight / ((fromIntegral ys :: Double) - 1)
        xv = vpRight .^ (((fromIntegral x :: Double) * pixelWidth) - halfWidth)
        yv = vpUp .^ (((fromIntegral y :: Double) * pixelHeight) - halfHeight)

viewPixelsAt :: Int -> Int -> Int -> Int -> [(Int, Int)]
viewPixelsAt xs ys x y
    | y >= ys = []
    | x >= xs && y < ys = viewPixelsAt xs ys 0 (y+1)
    | otherwise = (x, y) : viewPixelsAt xs ys (x+1) y

viewPixels :: Int -> Int -> [(Int, Int)]
viewPixels xs ys = viewPixelsAt xs ys 0 0
viewRays xs ys = fmap (\(x, y) -> (x, y, rayAt' xs ys x y)) $ viewPixels xs ys

view :: Int -> Int -> [(Int, Int, Colour)]
view xs ys = fmap (\(x, y, ray) -> (x, y, trace ray)) $ viewRays xs ys

someFunc :: IO ()
someFunc = do
    let picture = view (width :: Int) (height :: Int)
    --pPrint picture -- Print out the image data structure
    renderPicture picture


gradient from to t = (from .^ t') <+> (to .^ (1-t'))
    where
        t' = max 0.0 (min 1.0 t)


intersectSphere :: Sphere -> Ray -> Maybe CVec3
intersectSphere (Sphere sphereCentre sphereRadius) (Ray rayOrigin rayLine)
    = if hasIntersection then Just intersectionPoint else Nothing
    where
        -- https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
        -- rayLine must be unit
        rayLine' = normalize rayLine

        -- calculating the discriminant b2 - 4ac
        oc = rayOrigin <-> sphereCentre
        b = (rayLine' .* oc)
        discriminant = (b * b) - (oc .* oc) + (sphereRadius * sphereRadius)
        hasIntersection = discriminant > 0

        -- the smaller soluction to the line equation: rayOrigin + d*rayLine
        d = (-b) - sqrt discriminant
        intersectionPoint = rayOrigin <+> (rayLine' .^ d)


-- The Scene
sphere1 = Sphere (CVec3 0.0 3.5 (-3.0)) 3.0

-- Trace
trace :: Ray -> Colour
trace ray@(Ray origin dir@(CVec3 xd yd zd)) = case intersectSphere sphere1 ray of
    Just point -> point .^ 0.25
    Nothing -> gradient sunsetRed skyBlue yd


-- Utility functions
eventIsPressQ ev = case SDL.eventPayload ev of
    SDL.KeyboardEvent keyboardEvent ->
        SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    _ -> False

renderPicture img = do
    window <- SDL.createWindow "App" SDL.defaultWindow
    Exception.catch (runApp window) (catchErr window)
    where
        runApp window = do
            renderer <- SDL.createRenderer window (-1) (SDL.RendererConfig { SDL.rendererType = SDL.UnacceleratedRenderer, SDL.rendererTargetTexture = False })
            --rendererInfo <- SDL.getRendererInfo renderer
            --putStrLn $ show rendererInfo
            appLoop renderer img
        catchErr window exception = do
            SDL.destroyWindow window
            putStrLn "Caught exception!"
            putStrLn $ show (exception :: Exception.ErrorCall)


drawPoint renderer (x, y, CVec3 r g b) = do
    let drawColour = SDL.rendererDrawColor renderer
    drawColour SDL.$= (SDL.V4
                        (floor (r*255.0))
                        (floor (g*255.0))
                        (floor (b*255.0))
                        (255))
    SDL.drawPoint renderer (SDL.P (SDL.V2 (fromIntegral x :: CInt) (fromIntegral y :: CInt)))

appLoop renderer img = do
    mapM_ (drawPoint renderer) img
    SDL.present renderer
    events <- SDL.pollEvents
    let qPressed = any eventIsPressQ events
    threadDelay (16*1000)
    unless qPressed (appLoop renderer img)


