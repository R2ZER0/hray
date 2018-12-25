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

width = 200
height = 100

white = CVec3 1.0 1.0 1.0
skyBlue = (CVec3 135 206 235) .^ (1.0 / 255.0)

vec x y z = CVec3 x y z

-- Ray origin direction
data Ray = Ray CVec3 CVec3 deriving (Show, Eq)

lowerLeft = vec (-0.5) (-0.5) 1.0
xUnit xs = vec (1.0/(fromIntegral xs)) 0 0
yUnit ys = vec 0 (1.0/(fromIntegral ys)) 0

cameraLocation = vec 0 0 1

rayAt xs ys x y = Ray cameraLocation (lowerLeft <+> ((xUnit xs) .^ (fromIntegral x)) <+> ((yUnit ys) .^ (fromIntegral y)))

viewPixelsAt xs ys x y
    | x >= xs && y >= ys = []
    | x >= xs && y < ys = viewPixelsAt xs ys 0 (y+1)
    | otherwise = (x, y) : viewPixelsAt xs ys (x+1) y

viewPixels xs ys = viewPixelsAt xs ys 0 0

viewRays xs ys = fmap (\(x, y) -> (x, y, rayAt xs ys x y)) $ viewPixels xs ys

trace (Ray origin dir@(CVec3 xd yd zd)) = gradient white skyBlue yd

view xs ys = fmap (\(x, y, ray) -> (x, y, trace ray)) $ viewRays xs ys

someFunc :: IO ()
someFunc = do
    let picture = view (width :: Int) (height :: Int)
    --putStrLn $ show picture -- Print out the image data structure
    renderPicture picture


gradient from to t = (from .^ t') <+> (to .^ (1-t'))
    where
        t' = max 0.0 (min 1.0 t)

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


