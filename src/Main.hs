module Main (main) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Exception (finally)
import Control.Monad     (liftM2, unless, when)

import Graphics.Rendering.OpenGL (($=))

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Graphics.Fonts.OpenGL.Basic4x6

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = withGraphics showFont

showFont :: IO ()
showFont = do
    GLFW.resetTime

    quitting <- liftM2 (||) (GLFW.keyIsPressed GLFW.KeyEsc)
                            (GLFW.keyIsPressed (GLFW.CharKey 'Q'))

    GL.clear [GL.ColorBuffer]
    GL.color (GL.Color4 p1 p1 p1 p1)

    -- display digits
    GL.loadIdentity
    GL.ortho2D n50 p50 n50 p50

    GL.translate (GL.Vector3 (n50 + 4) (p50 - 4) 0)
    mapM_
      (>> GL.translate (GL.Vector3 p6 0 0))
      [digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9]

    -- display symbols
    GL.loadIdentity
    GL.ortho2D n50 p50 n50 p50

    GL.translate (GL.Vector3 (n50 + 4) (p50 - 12) 0)
    mapM_
      (>> GL.translate (GL.Vector3 p6 0 0))
      [exclamation, percent, plus, minus, period, slash, colon, lessThan, equal, greaterThan]

    -- display "2+2=4"
    GL.loadIdentity
    GL.ortho2D n100 p100 n100 p100

    GL.translate (GL.Vector3 (n100 + 6) (p100 - 50) 0)
    mapM_
      (>> GL.translate (GL.Vector3 p6 0 0))
      [digit2, plus, digit2, equal, digit4]

    -- display "97.3%"
    GL.loadIdentity
    GL.ortho2D n100 p100 n100 p100

    GL.translate (GL.Vector3 (n100 + 6) (p100 - 60) 0)
    mapM_
      (>> GL.translate (GL.Vector3 p6 0 0))
      [digit9, digit7, period, digit3, percent]

    GLFW.swapBuffers

    t <- fmap (secondsPerFrame -) GLFW.getTime
    when (t > 0) (GLFW.sleep t)

    unless quitting showFont
  where
    secondsPerFrame = recip (fromIntegral framesPerSecond)
    framesPerSecond = 30 :: Integer

    p1   = 1   :: GL.GLdouble
    p6   = 6   :: GL.GLdouble
    p50  = 50  :: GL.GLdouble
    p100 = 100 :: GL.GLdouble
    n50  = negate p50
    n100 = negate p100

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

withGraphics :: IO a -> IO a
withGraphics action =
    (startGraphics >> action) `finally` stopGraphics

startGraphics :: IO ()
startGraphics = do
    _ <- GLFW.initialize
    _ <- GLFW.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_width          = 600
      , GLFW.displayOptions_height         = 600
      , GLFW.displayOptions_numRedBits     = 5
      , GLFW.displayOptions_numGreenBits   = 5
      , GLFW.displayOptions_numBlueBits    = 5
      , GLFW.displayOptions_numFsaaSamples = Just 4
      }
    GL.blend       $= GL.Enabled
    GL.blendFunc   $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.multisample $= GL.Enabled

stopGraphics :: IO ()
stopGraphics = do
    GLFW.closeWindow
    GLFW.terminate
