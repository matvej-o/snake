module GameState where

import                        Control.Monad.IO.Class                    (liftIO)
import                        Data.Maybe                                (fromMaybe)
import                        System.Random

import                        Constants
import                        Direction
import                        Snake
import                        Input

newtype Fruit = Fruit Position deriving Show

instance Pos Fruit where
    position (Fruit pos) = pos

randomFruitPosition :: Snake -> [Fruit] -> IO Position
randomFruitPosition snake fruits = do
                                     let occupiedPositions = (Prelude.++) (snakeSegments snake) $ position <$> fruits
                                     x <- getStdRandom (randomR (0, fst dimensions - 1))
                                     y <- getStdRandom (randomR (0, snd dimensions - 1))
                                     let randomPosition = (x, y)
                                     if randomPosition `elem` occupiedPositions then randomFruitPosition snake fruits else pure randomPosition    

findAndEject :: (a -> Bool) -> [a] -> (Maybe a, [a])
findAndEject _ [] = (Nothing, [])
findAndEject p (x:xs)
    | p x       = (Just x, xs)
    | otherwise = (x:) <$> findAndEject p xs
    
data GameState = GameState Snake [Fruit] Integer
data Event = SnakeUpdate Snake
           | FruitEaten Fruit [Fruit] 
           | GameOver deriving Show

getScore :: GameState -> Integer
getScore (GameState _ _ score) = score

update :: Input -> GameState -> Event
update input (GameState snake fruits _) 
    | selfIntersects snake' = GameOver
    | otherwise = case maybeFruit of
                                    Just fruit -> FruitEaten  fruit fruits'
                                    Nothing    -> SnakeUpdate snake'
    where (maybeFruit, fruits') = findAndEject (collides snake') fruits
          snake' = moveSnake input snake
          
updateLogic win gameState@(GameState snake fruits score) input = do
        let event = update input gameState
        case event of
            SnakeUpdate snake' -> pure $ (GameState snake' fruits score, False)
            FruitEaten fruit fruits' -> do
                let direction = fromMaybe (headDirection snake) input
                fruitPosition <- liftIO $ randomFruitPosition snake fruits
                pure $ (GameState (Snake (position fruit) direction (Just snake)) (Fruit fruitPosition : fruits') $ score + 1, False)
            GameOver -> pure (gameState, True)