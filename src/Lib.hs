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

width = 400
height = width `div` 2

-- Colours
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

cameraLocation = vec 0 0 2

rayAt xs ys x y = Ray cameraLocation (normalize (lowerLeft <+> ((xUnit xs) .^ (fromIntegral x)) <+> ((yUnit ys) .^ (fromIntegral y))))

viewPixelsAt xs ys x y
    | x >= xs && y >= ys = []
    | x >= xs && y < ys = viewPixelsAt xs ys 0 (y+1)
    | otherwise = (x, y) : viewPixelsAt xs ys (x+1) y

viewPixels xs ys = viewPixelsAt xs ys 0 0
viewRays xs ys = fmap (\(x, y) -> (x, y, rayAt xs ys x y)) $ viewPixels xs ys
view xs ys = fmap (\(x, y, ray) -> (x, y, trace ray)) $ viewRays xs ys

someFunc :: IO ()
someFunc = do
    let picture = view (width :: Int) (height :: Int)
    --putStrLn $ show picture -- Print out the image data structure
    renderPicture picture


gradient from to t = (from .^ t') <+> (to .^ (1-t'))
    where
        t' = max 0.0 (min 1.0 t)

intersectSphere :: Sphere -> Ray -> Maybe (Double, CVec3)
intersectSphere (Sphere sphereCentre sphereRadius) (Ray rayOrigin rayDirection) =
    if discriminant < 0 then Nothing else Just (t, rayOrigin <+> (rayDirection .^ t))
    where
        cameraToCentre = rayOrigin <-> sphereCentre
        v = cameraToCentre .* rayOrigin
        eoDot = cameraToCentre .* cameraToCentre
        discriminant = (sphereRadius * sphereRadius) - eoDot + (v * v)
        t = v - (sqrt discriminant)

-- The Scene
sphere1 = Sphere (CVec3 0.0 0.0 (-2.5)) 0.05

-- Trace
trace ray@(Ray origin dir@(CVec3 xd yd zd)) = case intersectSphere sphere1 ray of
    Just (t, point) -> red
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
    unless qPressed (appLoop renderer img)


