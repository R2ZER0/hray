{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Vec3
import Text.Pretty.Simple (pPrint, pShow)
import qualified SDL as SDL
import qualified Control.Exception as Exception

type Vec = CVec3
vec x y z = CVec3 x y z

-- Ray origin direction
data Ray = Ray Vec Vec deriving (Show, Eq)

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


trace (Ray origin dir) = vec 1.0 0.0 0.0

view xs ys = fmap (\(x, y, ray) -> trace ray) $ viewRays xs ys

someFunc :: IO ()
someFunc = do
    let picture = view (2 :: Int) (2 :: Int)
    putStrLn $ show picture
    renderPicture picture

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
            appLoop renderer
        catchErr window exception = do
            SDL.destroyWindow window
            putStrLn "Caught exception!"
            putStrLn $ show (exception :: Exception.ErrorCall)
    

appLoop renderer = do
    events <- SDL.pollEvents
    let qPressed = any eventIsPressQ events
    unless qPressed (appLoop renderer)


