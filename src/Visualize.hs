module Visualize where

import                        Data.Int                                  (Int32)
import                        Graphics.GPipe
import                        Prelude                                           hiding (Either, Left, Right)

import                        Constants
import                        Snake
import                        GameState
import                        Direction

visualize :: Snake -> [Fruit] -> [(V4 Float, Int32)]
visualize (Snake pos dir snakes) fruits = (visualizeHead pos dir) ++ (visualizeTail snakes dir) ++ (visualizeFruit <$> fruits)
        where visualizeHead (x, y) dir = let (x', y') = move dir (x,y) in [((V4 (convertX x) (convertY y) (fst headTexOffset) (snd headTexOffset)) 
                                                                          , convertDirection dir)
                                                                          , ((V4 (convertX x') (convertY y') (fst tongueTexOffset) (snd tongueTexOffset)), convertDirection dir)]
              visualizeTail Nothing _ = []
              visualizeTail (Just (Snake (x, y) dir snakes)) previousDir
                | dir == previousDir = ((V4 (convertX x) (convertY y) (fst tailTexOffset) (snd tailTexOffset)), convertDirection dir) : visualizeTail snakes dir
                | otherwise          = ((V4 (convertX x) (convertY y) (fst cornerTexOffset) (snd cornerTexOffset)), combineDirections dir previousDir) : visualizeTail snakes dir
              visualizeFruit (Fruit (x, y)) = ((V4 (convertX x) (convertY y) (fst fruitTexOffset) (snd fruitTexOffset)), 0 :: Int32)
              convertDirection Up    = 0 :: Int32
              convertDirection Left  = 1
              convertDirection Down  = 2
              convertDirection Right = 3
              combineDirections Up Left     = 0
              combineDirections Up Right    = 1
              combineDirections Down Left   = 3
              combineDirections Down Right  = 2
              combineDirections Left Down   = 1
              combineDirections Left Up     = 2
              combineDirections Right Down  = 0
              combineDirections Right Up    = 3
              combineDirections _ _         = 0

vizualizeScore :: Integer -> [(V4 Float, Int32)]
vizualizeScore score = [ format (5,  13) $ toSprite 'G'
                       , format (6,  13) $ toSprite 'A'
                       , format (7,  13) $ toSprite 'M'
                       , format (8,  13) $ toSprite 'E'
                       , format (9,  13) $ toSprite ' '
                       , format (10, 13) $ toSprite 'O'
                       , format (11, 13) $ toSprite 'V'
                       , format (12, 13) $ toSprite 'E'
                       , format (13, 13) $ toSprite 'R'
                       , format (14, 13) $ toSprite '!'
                       , format (7,   9) $ toSprite 'S'
                       , format (8,   9) $ toSprite 'C'
                       , format (9,   9) $ toSprite 'O'
                       , format (10,  9) $ toSprite 'R'
                       , format (11,  9) $ toSprite 'E'
                       , format (8,   7) $ digitToSprite $ score `div` 100
                       , format (9,   7) $ digitToSprite $ score `div` 10 `mod` 10
                       , format (10,  7) $ digitToSprite $ score `mod` 10
                       ]
        where format (x, y) (texX, texY) = (V4 (convertX x) (convertY y) (texX/256) (texY/256), (0 :: Int32))
              toSprite 'G' = (  0, 40)
              toSprite 'A' = ( 40, 40)
              toSprite 'M' = ( 80, 40)
              toSprite 'E' = (120, 40)
              toSprite 'O' = (160, 40)
              toSprite 'V' = (200, 40)
              toSprite 'R' = (  0, 80)
              toSprite '!' = ( 40, 80)
              toSprite 'S' = ( 80, 80)
              toSprite 'C' = (120, 80)
              toSprite ' ' = (160, 80)
              toSprite  _  = toSprite ' ' 
              
              digitToSprite 0 = (  0, 120)
              digitToSprite 1 = ( 40, 120)
              digitToSprite 2 = ( 80, 120)
              digitToSprite 3 = (120, 120)
              digitToSprite 4 = (160, 120)
              digitToSprite 5 = (200, 120)
              digitToSprite 6 = (  0, 160)
              digitToSprite 7 = ( 40, 160)
              digitToSprite 8 = ( 80, 160)
              digitToSprite 9 = (120, 160)
              digitToSprite _ = toSprite ' '

convertX :: Int -> Float
convertX x = let xDimensionHalfed = (fromIntegral $ fst dimensions) / 2 in (fromIntegral x - xDimensionHalfed) / xDimensionHalfed

convertY :: Int -> Float
convertY y = let yDimensionHalfed = (fromIntegral $ snd dimensions) / 2 in (fromIntegral y - yDimensionHalfed) / yDimensionHalfed