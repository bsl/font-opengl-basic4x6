module Graphics.Fonts.OpenGL.Basic4x6
  ( -- * Digits
    digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9
    -- * Symbols
  , exclamation, percent, plus, minus, period, slash, colon, lessThan, equal, greaterThan
  )
  where

import qualified Graphics.Rendering.OpenGL as GL

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9 :: IO ()

digit0 =
    renderQuads
      [ (-2,3),  (-2,-3), (-1,-3), (-1,3)
      , (-1,3),  (-1,2),  (1,2),   (1,3)
      , (-1,-2), (-1,-3), (1,-3),  (1,-2)
      , (1,3),   (1,-3),  (2,-3),  (2,3)
      ]

digit1 =
    renderQuads
      [ (-0.5,3), (-0.5,-3), (0.5,-3), (0.5,3)
      ]

digit2 =
    renderQuads
      [ (-2,3),  (-2,2),  (1,2),   (1,3)
      , (1,3),   (1,-1),  (2,-1),  (2,3)
      , (-1,0),  (-1,-1), (1,-1),  (1,0)
      , (-2,0),  (-2,-3), (-1,-3), (-1,0)
      , (-1,-2), (-1,-3), (2,-3),  (2,-2)
      ]

digit3 =
    renderQuads
      [ (-2,3),  (-2,2),  (1,2),  (1,3)
      , (1,3),   (1,-3),  (2,-3), (2,3)
      , (-2,0),  (-2,-1), (1,-1), (1,0)
      , (-2,-2), (-2,-3), (1,-3), (1,-2)
      ]

digit4 =
    renderQuads
      [ (-2,3), (-2,-1), (-1,-1), (-1,3)
      , (-1,0), (-1,-1), (1,-1),  (1,0)
      , (1,3),  (1,-3),  (2,-3),  (2,3)
      ]

digit5 =
    renderQuads
      [ (-1,3),  (-1,2),  (2,2),   (2,3)
      , (-2,3),  (-2,-1), (-1,-1), (-1,3)
      , (-1,0),  (-1,-1), (1,-1),  (1,0)
      , (1,0),   (1,-3),  (2,-3),  (2,0)
      , (-2,-2), (-2,-3), (1,-3),  (1,-2)
      ]

digit6 =
    renderQuads
      [ (-1,3),  (-1,2),  (2,2),   (2,3)
      , (-2,3),  (-2,-3), (-1,-3), (-1,3)
      , (-1,0),  (-1,-1), (1,-1),  (1,0)
      , (-1,-2), (-1,-3), (1,-3),  (1,-2)
      , (1,0),   (1,-3),  (2,-3),  (2,0)
      ]

digit7 =
    renderQuads
      [ (-2,3), (-2,2), (1,2),  (1,3)
      , (1,3),  (1,-3), (2,-3), (2,3)
      ]

digit8 =
    renderQuads
      [ (-2,3),  (-2,-3), (-1,-3), (-1,3)
      , (-1,3),  (-1,2),  (1,2),   (1,3)
      , (-1,0),  (-1,-1), (1,-1),  (1,0)
      , (-1,-2), (-1,-3), (1,-3),  (1,-2)
      , (1,3),   (1,-3),  (2,-3),  (2,3)
      ]

digit9 =
    renderQuads
      [ (-2,3), (-2,-1), (-1,-1), (-1,3)
      , (-1,3), (-1,2),  (1,2),   (1,3)
      , (-1,0), (-1,-1), (1,-1),  (1,0)
      , (1,3),  (1,-3),  (2,-3),  (2,3)
      ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

exclamation, percent, plus, minus, period, slash, colon, lessThan, equal, greaterThan :: IO ()

exclamation =
    renderQuads
      [ (-0.5,3),  (-0.5,-1), (0.5,-1), (0.5,3)
      , (-0.5,-2), (-0.5,-3), (0.5,-3), (0.5,-2)
      ]

percent =
    renderQuads
      [ (1,3),  (-2,-3), (-1,-3), (2,3)
      , (-2,3), (-2,2),  (-1,2),  (-1,3)
      , (1,-2), (1,-3),  (2,-3),  (2,-2)
      ]

plus =
    renderQuads
      [ (-2,0.5),  (-2,-0.5),  (-0.5,-0.5), (-0.5,0.5)
      , (-0.5,2),  (-0.5,-2),  (0.5,-2),    (0.5,2)
      , (0.5,0.5), (0.5,-0.5), (2,-0.5),    (2,0.5)
      ]

minus =
    renderQuads
      [ (-2,0.5), (-2,-0.5), (2,-0.5), (2,0.5)
      ]

period =
    renderQuads
      [ (-0.5,-2), (-0.5,-3), (0.5,-3), (0.5,-2)
      ]

slash =
    renderQuads
      [ (1,3), (-2,-3), (-1,-3), (2,3)
      ]

colon =
    renderQuads
      [ (-0.5,1.5),  (-0.5,0.5),  (0.5,0.5),  (0.5,1.5)
      , (-0.5,-0.5), (-0.5,-1.5), (0.5,-1.5), (0.5,-0.5)
      ]

lessThan =
    renderQuads
      [ (1,3),  (-2,0), (-1,0), (2,3)
      , (-2,0), (1,-3), (2,-3), (-1,0)
      ]

equal =
    renderQuads
      [ (-2,1.5),  (-2,0.5),  (2,0.5),  (2,1.5)
      , (-2,-0.5), (-2,-1.5), (2,-1.5), (2,-0.5)
      ]

greaterThan =
    renderQuads
      [ (-2,3), (1,0),   (2,0),   (-1,3)
      , (1,0),  (-2,-3), (-1,-3), (2,0)
      ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

renderQuads :: [(Double,Double)] -> IO ()
renderQuads =
    GL.renderPrimitive GL.Quads . mapM_ (\(x, y) ->
                                          let x' = realToFrac x :: GL.GLfloat
                                              y' = realToFrac y :: GL.GLfloat
                                          in GL.vertex (GL.Vertex2 x' y')
                                        )
