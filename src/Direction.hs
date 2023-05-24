module Direction (Position, Direction(..), isOppositeOf, collides, move, Pos(..)) where

import                        Prelude                                                   hiding (Either, Right, Left)

import Constants

type Position = (Int, Int) -- x and y coordinates of something

normalize :: Position -> Position
normalize pos = ((fst pos) `mod` (fst dimensions), (snd pos) `mod` (snd dimensions))

class Pos a where
    position :: a -> Position

data Direction = Up | Right | Left | Down deriving (Eq, Show)

isOppositeOf :: Direction -> Direction -> Bool
isOppositeOf Right Left  = True
isOppositeOf Left  Right = True
isOppositeOf Up    Down  = True
isOppositeOf Down  Up    = True
isOppositeOf _     _     = False

collides :: (Pos a, Pos b) => a -> b -> Bool
collides x y = position x == position y

move' :: Direction -> Position -> Position
move' Up    (x, y) = (x, y + 1)
move' Right (x, y) = (x + 1, y)
move' Left  (x, y) = (x - 1, y)
move' Down  (x, y) = (x, y - 1)

move :: Direction -> Position -> Position
move dir pos = normalize $ move' dir pos