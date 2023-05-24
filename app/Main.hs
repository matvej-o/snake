{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Main where

import                        Control.Monad.IO.Class                    (liftIO)
import                        Graphics.GPipe                                            
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW       as GLFW
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW.Input as GLFW
import                        Graphics.UI.GLFW                          (WindowHint(..))
import                        Codec.Picture
import                        Data.Word                                 (Word8)
import                        Data.Int                                  (Int32)
import                        Control.Applicative                       (pure)

import                        Constants
import                        Loops
import                        Snake
import                        GameState
import                        Direction                                 (Direction(Up))

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
        



getTexture :: FilePath -> IO [(V4 Word8)]
getTexture filePath = readPng filePath >>= \x -> case x of
        Prelude.Left  _            -> error "YOU FORGOT YOUR SPRITES MATE"
        Prelude.Right dynamicImage -> pure . fmap pixelToVector . extractPixels . convertRGBA8 $ dynamicImage
                where pixelToVector (PixelRGBA8 r g b a) = V4 r g b a -- r, g, b, a are just Word8 values
                      extractPixels image = [ pixelAt image x y | y <- [0..(imageHeight image - 1)], x <- [0..(imageWidth image - 1)] ] 
                      -- EXTREMELY SUGARY