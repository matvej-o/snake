{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Loops where

import                        Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import                        Control.Applicative                       (pure)
import                        Control.Monad                             (unless)
import                        Control.Monad.IO.Class                    (liftIO)
import                        Data.Maybe                                (fromMaybe)

import                        Constants
import                        Render
import                        Input
import                        GameState

loop gameState win shader shaderGameOver quadBuffer spriteBuffer gameOverBuffer previousExcessTime previousTime lastInput = do
        maybeCurrentTime <- liftIO $ GLFW.getTime
        input <- getInput win lastInput
        let currentTime = fromMaybe previousTime maybeCurrentTime
            timeElapsed = currentTime - previousTime
            accumulatedTime = previousExcessTime + timeElapsed
         
        (gameState', gameOver, accumulatedTime') <- if (accumulatedTime < logicUpdateTime) 
                                            then pure (gameState, False, accumulatedTime) 
                                            else (\(x, y) -> (x, y, accumulatedTime - logicUpdateTime)) <$> updateLogic win gameState input
        
        renderState gameState' win shader quadBuffer spriteBuffer
        closeRequested <- GLFW.windowShouldClose win
        unless (closeRequested == Just True) $ if gameOver 
                then gameOverAnimationLoop gameState' win shader shaderGameOver quadBuffer spriteBuffer gameOverBuffer (1 :: Float) (0 :: Seconds) currentTime
                else loop gameState' win shader shaderGameOver quadBuffer spriteBuffer gameOverBuffer accumulatedTime' currentTime input
                
gameOverAnimationLoop gameState@(GameState _ _ score) win shader shaderGameOver quadBuffer spriteBuffer gameOverBuffer (lowerVertex :: Float) previousExcessTime previousTime = do
        maybeCurrentTime <- liftIO $ GLFW.getTime
        let currentTime = fromMaybe previousTime maybeCurrentTime
            timeElapsed = currentTime - previousTime
            accumulatedTime = previousExcessTime + timeElapsed
            (lowerVertex', accumulatedTime') = if (accumulatedTime < (6*logicUpdateTime)) 
                then (lowerVertex, accumulatedTime) 
                else (lowerVertex - 0.4, accumulatedTime - (3*logicUpdateTime))
        gameOverAnimation gameState win shader shaderGameOver quadBuffer spriteBuffer gameOverBuffer lowerVertex'
        closeRequested <- GLFW.windowShouldClose win
        unless (closeRequested == Just True) $ if lowerVertex < (-0.99)
                then showScoreLoop win shader quadBuffer spriteBuffer score
                else gameOverAnimationLoop gameState win shader shaderGameOver quadBuffer spriteBuffer gameOverBuffer lowerVertex' accumulatedTime' currentTime

showScoreLoop win shader quadBuffer spriteBuffer score = do
        showScore win shader quadBuffer spriteBuffer score
        closeRequested <- GLFW.windowShouldClose win
        unless (closeRequested == Just True) $ showScoreLoop win shader quadBuffer spriteBuffer score