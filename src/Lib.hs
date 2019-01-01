{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Lib
    ( runAppWithCatch
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

width = 600
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

-- Camera location fov vector
data Camera = Camera CVec3 Double CVec3


-- Scene objects
data Scene = Scene Sphere

lowerLeft = vec (-0.5) (-0.5) 1.0
xUnit xs = vec (1.0/(fromIntegral xs)) 0 0
yUnit ys = vec 0 (1.0/(fromIntegral ys)) 0

camera :: Camera
camera = Camera cameraLocation cameraFov cameraVector
  where
    cameraLocation = vec 0.0 1.8 10.0   
    cameraFov = 45.0
    cameraVector = vec 0.0 3.0 0.0

--rayAt xs ys x y = Ray cameraLocation (normalize (lowerLeft <+> ((xUnit xs) .^ (fromIntegral x)) <+> ((yUnit ys) .^ (fromIntegral y))))

rayAt' :: Camera -> Int -> Int -> Int -> Int -> Ray
rayAt' (Camera cameraLocation cameraFov cameraVector) xs ys x y = Ray cameraLocation (normalize (eyeVector <+> yv <+> xv))
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

viewRays :: Camera -> Int -> Int -> [(Int, Int, Ray)]
viewRays camera xs ys = fmap (\(x, y) -> (x, y, rayAt' camera xs ys x y)) $ viewPixels xs ys

render :: Scene -> Camera -> Int -> Int -> [(Int, Int, Colour)]
render scene camera xs ys = fmap (\(x, y, ray) -> (x, y, trace scene ray)) $ viewRays camera xs ys

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
trace :: Scene -> Ray -> Colour
trace (Scene sphere1) ray@(Ray origin dir@(CVec3 xd yd zd)) = case intersectSphere sphere1 ray of
    Just point -> point .^ 0.25
    Nothing -> gradient sunsetRed skyBlue yd

-- Update functions

moveCamera :: Camera -> CVec3 -> Camera
moveCamera (Camera point fov v) delta = Camera (point <+> delta) fov v

-- Utility functions
eventIsPress key ev = case SDL.eventPayload ev of
    SDL.KeyboardEvent keyboardEvent ->
        SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == key
    _ -> False

runAppWithCatch :: IO ()
runAppWithCatch = do
    window <- SDL.createWindow "App" SDL.defaultWindow
    Exception.catch (runApp window) (catchErr window)
    where
        runApp window = do
            renderer <- SDL.createRenderer window (-1) (SDL.RendererConfig { SDL.rendererType = SDL.UnacceleratedRenderer, SDL.rendererTargetTexture = False })
            --rendererInfo <- SDL.getRendererInfo renderer
            --putStrLn $ show rendererInfo
            let scene = Scene sphere1
            appLoop renderer scene camera
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

showPicture renderer !img = do
    mapM_ (drawPoint renderer) img
    SDL.present renderer

appLoop renderer scene camera = do
    let picture = render scene camera (width :: Int) (height :: Int)
    showPicture renderer picture
    events <- SDL.pollEvents
    let pressed key = any (eventIsPress key) events
    --threadDelay ((1000*1000) `div` 60)
    let camera' = if pressed SDL.KeycodeDown then moveCamera camera (vec 0 0.05 0.2) 
                  else if pressed SDL.KeycodeUp then moveCamera camera (vec 0 (-0.05) (-0.2))
                  else if pressed SDL.KeycodeLeft then moveCamera camera (vec (-0.2) 0 0)
                  else if pressed SDL.KeycodeRight then moveCamera camera (vec 0.2 0 0)
                  else camera 
    unless (pressed SDL.KeycodeQ) (appLoop renderer scene camera')


