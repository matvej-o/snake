{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Render where

import                        Data.Int                                  (Int32)
import                        Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW       as GLFW
import                        Control.Applicative                       (pure)

import GameState
import Snake
import Visualize

renderState :: (ContextHandler ctx) 
                => GameState 
                -> Window os RGBAFloat ds 
                -> (PrimitiveArray Triangles (B4 Float, B4 Float, B Int32) -> Render os ()) 
                -> Buffer os (B4 Float) 
                -> Buffer os (B4 Float, B Int32) 
                -> ContextT ctx os IO ()
renderState gameState@(GameState snake fruits score) win shader quadBuffer spriteBuffer = do
        let sprites = visualize snake fruits
        writeBuffer spriteBuffer 0 sprites
        render $ do
            clearWindowColor win 0.5
            quadVertices <- newVertexArray quadBuffer
            spriteArray <- newVertexArray spriteBuffer >>= pure . takeVertices (length sprites)
            shader $ toPrimitiveArrayInstanced TriangleStrip (\x (y, z) -> (x, y, z)) quadVertices spriteArray
            
        swapWindowBuffers win

gameOverAnimation :: (ContextHandler ctx) 
                    => GameState 
                    -> Window os RGBAFloat ds 
                    -> (PrimitiveArray Triangles (B4 Float, B4 Float, B Int32) -> Render os ())
                    -> (PrimitiveArray Triangles (B4 Float, B4 Float) -> Render os ())
                    -> Buffer os (B4 Float)
                    -> Buffer os (B4 Float, B Int32) 
                    -> Buffer os (B4 Float, B4 Float)
                    -> Float
                    -> ContextT ctx os IO ()        
gameOverAnimation gameState@(GameState snake fruits score) win shader shaderGameOver quadBuffer spriteBuffer gameOverBuffer lowerVertex = do
        let sprites = visualize snake fruits
        writeBuffer spriteBuffer 0 sprites
        writeBuffer gameOverBuffer 0 [ (V4   1  lowerVertex  0 1, V4 0 0 1 1)
                                     , (V4   1  2            0 1, V4 0 0 1 1)
                                     , (V4 (-1) lowerVertex  0 1, V4 0 0 1 1)
                                     , (V4 (-1) 2            0 1, V4 0 0 1 1)
                                     ]
        render $ do
            clearWindowColor win 0.5
            quadVertices <- newVertexArray quadBuffer
            spriteArray <- newVertexArray spriteBuffer >>= pure . takeVertices (length sprites)
            gameOverArray <- newVertexArray gameOverBuffer
            shader $ toPrimitiveArrayInstanced TriangleStrip (\x (y, z) -> (x, y, z)) quadVertices spriteArray
            shaderGameOver $ toPrimitiveArray TriangleStrip gameOverArray
            
        swapWindowBuffers win

showScore :: (ContextHandler ctx) 
                => Window os RGBAFloat ds 
                -> (PrimitiveArray Triangles (B4 Float, B4 Float, B Int32) -> Render os ()) 
                -> Buffer os (B4 Float) 
                -> Buffer os (B4 Float, B Int32) 
                -> Integer
                -> ContextT ctx os IO ()
showScore win shader quadBuffer spriteBuffer score = do
        let sprites = vizualizeScore score
        writeBuffer spriteBuffer 0 sprites
        render $ do
            clearWindowColor win (V4 0 0 1 1)
            quadVertices <- newVertexArray quadBuffer
            spriteArray <- newVertexArray spriteBuffer >>= pure . takeVertices (length sprites)
            shader $ toPrimitiveArrayInstanced TriangleStrip (\x (y, z) -> (x, y, z)) quadVertices spriteArray
        
        swapWindowBuffers win