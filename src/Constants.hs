module Constants where

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