{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Main where

import                        Prelude                                                   hiding (Either, Right, Left)
import qualified              Prelude                                   (Either(..))
import                        Control.Monad                             (unless)
import                        Control.Monad.IO.Class                    (liftIO)
import                        Data.Maybe                                (fromMaybe)
import                        System.Random
import                        Graphics.GPipe                                            hiding (normalize)
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW       as GLFW
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW.Input as GLFW
import                        Graphics.UI.GLFW                          (WindowHint(..))
import                        Codec.Picture
import                        Data.Word                                 (Word8)
import                        Data.Int                                  (Int32)
import                        Control.Applicative                       (pure)

--- CONSTANTS ---
dimensions :: (Int, Int)
dimensions = (20, 20)

type Seconds = Double
logicUpdateTime :: Seconds
logicUpdateTime = 0.167 -- about 1/6 of a second, or 10 frames assuming 60 fps

textureAtlasPath :: FilePath
textureAtlasPath = "sprites.png"

tailTexOffset :: (Float, Float)
tailTexOffset = (0, 0)
headTexOffset :: (Float, Float)
headTexOffset = (40/256, 0)
fruitTexOffset :: (Float, Float)
fruitTexOffset = (80/256, 0)
cornerTexOffset :: (Float, Float)
cornerTexOffset = (120/256, 0)
tongueTexOffset :: (Float, Float)
tongueTexOffset = (160/256, 0)
--- CONSTANTS ---

type Id a = a -> a

main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
        textureAtlas <- liftIO $ getTexture textureAtlasPath -- don't even allocate a window if there is no sprite file
        win <- newWindow (WindowFormatColor RGBA8) (GLFW.WindowConfig 800 800 "THE SNAKE GAME" Nothing [WindowHint'Resizable False] Nothing)
        quadBuffer :: Buffer os (B4 Float) <- newBuffer 4       -- a quad that will then be rotated, scaled, translated,
        writeBuffer quadBuffer 0 [ V4   0.5  (-0.5) 0 1         -- and textured to display sprites
                                 , V4   0.5    0.5  0 1
                                 , V4 (-0.5) (-0.5) 0 1
                                 , V4 (-0.5)   0.5  0 1
                                 ]
        --writeBuffer quadBuffer 0 [ V4 (-0.5)   0.5  0 1         -- and textured to display sprites
        --                         , V4   0.5    0.5  0 1
        --                         , V4 (-0.5) (-0.5) 0 1
        --                         , V4   0.5  (-0.5) 0 1
        --                         ]
        spriteBuffer :: Buffer os (B4 Float, B Int32) <- newBuffer 800 -- V2 Float for position offset + V2 Float for texture offset + Int for angle (value x represents angle pi*x/2)
        gameOverBuffer :: Buffer os (B4 Float, B4 Float) <- newBuffer 4
        
        texture <- newTexture2D RGBA8 (V2 256 256) 1
        writeTexture2D texture 0 0 (V2 256 256) textureAtlas -- here be loaded texture
        
        let snake = Snake (fst dimensions `div` 2, snd dimensions `div` 2) Up Nothing
        fruitPosition <- liftIO $ randomFruitPosition snake []
        let startingState = GameState snake [Fruit fruitPosition] 0
        
        shader <- compileShader $ do
                primitiveStream <- toPrimitiveStream (id :: Id (PrimitiveArray Triangles (B4 Float, B4 Float, B Int32)))
                let rotationMatrix a = V4 (V4 (cos a) (-sin a) 0 0)
                                          (V4 (sin a) (cos a)  0 0)
                                          (V4  0       0       1 0)
                                          (V4  0       0       0 1)
                let rotateVertex n vec = (rotationMatrix $ (toFloat n :: S V Float) * pi/2) !* vec
                let scaleVertex (V4 x y z d) scalingX scalingY = V4 (x * scalingX) (y * scalingY) z d
                let primitiveStream' = (\(quadVertex@(V4 qX qY _ _), (V4 x y texX texY), n) -> 
                        (scaleVertex (rotateVertex n quadVertex + (V4 0.5 0.5 0 0)) 0.1 0.1 + (V4 x y 0 0), V2 (texX + ((0.5 + qX)*40/256)) (texY + ((0.5 - qY)*40/256)))) 
                            <$> primitiveStream
                fragmentStream <- rasterize (const (Front, ViewPort (V2 0 0) (V2 800 800), DepthRange 0 1)) primitiveStream'
                let filter = SamplerFilter Nearest Nearest Nearest Nothing --SamplerNearest -- SamplerNearest is the only possible filter for RGBA8UI (RGBA with 8-byte unsigned integers for each channel)
                    edge = (pure Repeat, undefined)
                (samp :: Sampler2D (Format RGBAFloat)) <- newSampler2D (const (texture, filter, edge))
                
                --Earlier it was let sampleTexture = pure . sample2D samp SampleAuto (Just 3) Nothing; THAT PURE WAS REDUNDANT AND DRANK ALL MY BLOOD I HATE IT
                let sampleTexture = sample2D samp SampleAuto Nothing Nothing
                    fragmentStream' = sampleTexture <$> fragmentStream 
                    blending = BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero One) (V4 0 0 0 0)
                    -- Assumes that alpha is always either 1 or 0, draws only the parts of the sprite with non-zero alpha.
                drawWindowColor (const (win, ContextColorOption blending $ pure True)) (fragmentStream' :: FragmentStream (V4 (S F Float)))
                
        shaderGameOver <- compileShader $ do
                primitiveStream <- toPrimitiveStream (id :: Id (PrimitiveArray Triangles (B4 Float, B4 Float)))
                fragmentStream <- rasterize (const (Front, ViewPort (V2 0 0) (V2 800 800), DepthRange 0 1)) primitiveStream
                drawWindowColor (const (win, ContextColorOption NoBlending $ pure True)) fragmentStream
        
        loop startingState win shader shaderGameOver quadBuffer spriteBuffer gameOverBuffer (0 :: Seconds) (0 :: Seconds) Nothing
        
        
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

updateLogic win gameState@(GameState snake fruits score) input = do
        let event = update input gameState
        case event of
            SnakeUpdate snake' -> pure $ (GameState snake' fruits score, False)
            FruitEaten fruit fruits' -> do
                let direction = fromMaybe (headDirection snake) input
                fruitPosition <- liftIO $ randomFruitPosition snake fruits
                pure $ (GameState (Snake (position fruit) direction (Just snake)) (Fruit fruitPosition : fruits') $ score + 1, False)
            GameOver -> pure (gameState, true)

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
        

getTexture :: FilePath -> IO [(V4 Word8)]
getTexture filePath = readPng filePath >>= \x -> case x of
        Prelude.Left  _            -> error "YOU FORGOT YOUR SPRITES MATE"
        Prelude.Right dynamicImage -> pure . fmap pixelToVector . extractPixels . convertRGBA8 $ dynamicImage
                where pixelToVector (PixelRGBA8 r g b a) = V4 r g b a -- r, g, b, a are just Word8 values
                      extractPixels image = [ pixelAt image x y | y <- [0..(imageHeight image - 1)], x <- [0..(imageWidth image - 1)] ] 
                      -- EXTREMELY SUGARY, BUT I DON'T CARE AT THIS POINT
        

visualize :: Snake -> [Fruit] -> [(V4 Float, Int32)]
visualize (Snake pos dir snakes) fruits = (visualizeHead pos dir) ++ (visualizeTail snakes dir) ++ (visualizeFruit <$> fruits)
        where visualizeHead (x, y) dir = let (x', y') = move dir (x,y) in [((V4 (convertX x) (convertY y) (fst headTexOffset) (snd headTexOffset)), convertDirection dir)
                                                                          , ((V4 (convertX x') (convertY y') (fst tongueTexOffset) (snd tongueTexOffset)), convertDirection dir)]
              --visualizeTail ((x, y), dir) = ((V4 (convertX x) (convertY y) (fst tailTexOffset) (snd tailTexOffset)), convertDirection dir)
              --flatSnakes = flattenTail snakes
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

convertX x = let xDimensionHalfed = (fromIntegral $ fst dimensions) / 2 in (fromIntegral x - xDimensionHalfed) / xDimensionHalfed
convertY y = let yDimensionHalfed = (fromIntegral $ snd dimensions) / 2 in (fromIntegral y - yDimensionHalfed) / yDimensionHalfed

randomFruitPosition :: Snake -> [Fruit] -> IO Position
randomFruitPosition snake fruits = do
                                     let occupiedPositions = (Prelude.++) (snakeSegments snake) $ position <$> fruits
                                     x <- getStdRandom (randomR (0, fst dimensions - 1))
                                     y <- getStdRandom (randomR (0, snd dimensions - 1))
                                     let randomPosition = (x, y)
                                     if randomPosition `elem` occupiedPositions then randomFruitPosition snake fruits else pure randomPosition
                                     

normalize :: Position -> Position
normalize pos = ((fst pos) `mod` (fst dimensions), (snd pos) `mod` (snd dimensions))

type Position = (Int, Int) -- x and y coordinates of something
data Direction = Up | Right | Left | Down deriving (Eq, Show)

isOppositeOf :: Direction -> Direction -> Bool
isOppositeOf Right Left  = True
isOppositeOf Left  Right = True
isOppositeOf Up    Down  = True
isOppositeOf Down  Up    = True
isOppositeOf _     _     = False

class Pos a where
    position :: a -> Position


move' :: Direction -> Position -> Position
move' Up    (x, y) = (x, y + 1)
move' Right (x, y) = (x + 1, y)
move' Left  (x, y) = (x - 1, y)
move' Down  (x, y) = (x, y - 1)

move :: Direction -> Position -> Position
move dir pos = normalize $ move' dir pos

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

newtype Fruit = Fruit Position deriving Show

instance Pos Fruit where
    position (Fruit pos) = pos
    

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

type Input = Maybe Direction

collides :: (Pos a, Pos b) => a -> b -> Bool
collides x y = position x == position y

update :: Input -> GameState -> Event
update input (GameState snake fruits _) 
    | selfIntersects snake' = GameOver
    | otherwise = case maybeFruit of
                                    Just fruit -> FruitEaten  fruit fruits'
                                    Nothing    -> SnakeUpdate snake'
    where (maybeFruit, fruits') = findAndEject (collides snake') fruits
          snake' = moveSnake input snake
          