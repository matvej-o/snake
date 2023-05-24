{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Input where

import                        Graphics.GPipe                                            
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW       as GLFW
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW.Input as GLFW
import                        Prelude                                   hiding (Either, Left, Right)

import Direction

type Input = Maybe Direction

getInput :: Window os c ds -> Input -> ContextT GLFW.Handle os IO Input
getInput win lastInput = do
        let keys = [GLFW.Key'Up, GLFW.Key'Right, GLFW.Key'Left, GLFW.Key'Down]
        arePressed <- traverse (isPressed win) keys
        case arePressed of
            [True, _,    _,    _   ] -> pure $ Just Up
            [_,    True, _,    _   ] -> pure $ Just Right
            [_,    _,    True, _   ] -> pure $ Just Left
            [_,    _,    _,    True] -> pure $ Just Down
            _                        -> pure $ lastInput

isPressed :: Window os c ds -> GLFW.Key -> ContextT GLFW.Handle os IO Bool
isPressed win key = GLFW.getKey win key >>= \status -> case status of
        Just GLFW.KeyState'Pressed -> pure True
        _                          -> pure False