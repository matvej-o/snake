module Snake where

import Direction

data Snake = Snake Position Direction (Maybe Snake) deriving Show -- a non-empty list of snake tiles

instance Pos Snake where
    position (Snake pos _ _) = pos

headDirection :: Snake -> Direction
headDirection (Snake _ dir _) = dir

moveSnake :: Maybe Direction -> Snake -> Snake
moveSnake (Just dir) (Snake pos dir' snakes) 
    | isOppositeOf dir dir' =               moveSnake Nothing $ Snake pos dir' snakes
    | otherwise =                           Snake (move dir pos) dir $ moveSnake (Just dir') <$> snakes
moveSnake Nothing (Snake pos dir' snakes) = Snake (move dir' pos) dir' $ moveSnake (Just dir') <$> snakes

flattenTail :: Maybe Snake -> [(Position, Direction)]
flattenTail Nothing = []
flattenTail (Just (Snake pos dir snakes)) = (pos, dir) : flattenTail snakes

snakeSegments :: Snake -> [Position]
snakeSegments (Snake pos _ maybeSnake) = case maybeSnake of
                                         Just snake -> pos : (snakeSegments snake)
                                         Nothing -> [pos]

selfIntersects :: Snake -> Bool
selfIntersects = go []
    where go occupiedPositions (Snake pos _ Nothing) = pos `elem` occupiedPositions
          go occupiedPositions (Snake pos _ (Just snake)) = pos `elem` occupiedPositions || go (pos : occupiedPositions) snake