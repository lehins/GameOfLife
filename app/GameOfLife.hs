{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Massiv.Array as A
import Data.Word
import GameOfLife.Board
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

blinker :: Array S Ix2 Word8
blinker = [ [0, 1, 0]
          , [0, 1, 0]
          , [0, 1, 0] ]


glider :: Array S Ix2 Word8
glider = [ [0, 1, 0]
         , [0, 0, 1]
         , [1, 1, 1] ]

inf2 :: Array S Ix2 Word8
inf2 = [ [1, 1, 1, 0, 1]
       , [1, 0, 0, 0, 0]
       , [0, 0, 0, 1, 1]
       , [0, 1, 1, 0, 1]
       , [1, 0, 1, 0, 1] ]

data Gloss =
  Gloss
  { glossCellSize :: !Int
  , glossBoard    :: !LifeBoard
  , glossSize     :: !(Int, Int)
  , glossCounter  :: !Int
  }

aliveColor, deadColor :: Color
aliveColor = makeColor 0 0 1 1
deadColor = makeColor 1 1 1 1

runGameOfLife :: IO ()
runGameOfLife = do
  lifeBoard <- initLife (Sz2 480 640) glider
  let initBoard = initLifeBoard lifeBoard 0 (sizeToVisible initCellSize initSize)
      initGloss =
        Gloss
          { glossCellSize = initCellSize
          , glossBoard = initBoard
          , glossSize = initSize
          , glossCounter = itersUpdate
          }
  playIO disp bColor perSec initGloss lifeBoardToPicture onEvent boardStep
  where
    initCellSize = 1
    sizeToVisible cellSize (n, m) = Sz2 (m `div` cellSize) (n `div` cellSize)
    itersUpdate = 1
    perSec = 150
    bColor = deadColor
    initSize = (640, 480)
    initPosition = (500, 200)
    disp = InWindow "Conway's Game of Life" initSize initPosition
    onEvent event gloss@Gloss {..} =
      pure $
      case event of
        EventResize newSize ->
          let newVisibleSize = sizeToVisible glossCellSize newSize
           in gloss
                { glossSize = newSize
                , glossBoard = glossBoard {visibleSize = newVisibleSize}
                }
        _ -> gloss
    boardStep _ gloss@Gloss {..}
      | glossCounter == 0 = do
        newBoard <- lifeBoardStep glossBoard
        pure gloss {glossBoard = newBoard, glossCounter = itersUpdate}
      | otherwise = pure gloss {glossCounter = glossCounter - 1}


lifeBoardToPicture :: MonadIO m => Gloss -> m Picture
lifeBoardToPicture Gloss {..} = do
  boardSize <- getBoardSize glossBoard
  Pictures <$>
    mapAlive (toPolygons (liftIndex (`div` 2) (unSz boardSize))) glossBoard
  where
    toPolygons halfSize ix =
      case (ix - halfSize) * pureIndex glossCellSize of
        (i :. j) ->
          Color aliveColor $
          Polygon
            [ (fromIntegral x, fromIntegral y)
            | (x, y) <-
                [ (j, i)
                , (j + glossCellSize, i)
                , (j + glossCellSize, i + glossCellSize)
                , (j, i + glossCellSize)
                ]
            ]



-- sizeFromSz2 :: Sz2 -> G.Size
-- sizeFromSz2 (Sz2 m n) = Size (fromIntegral n) (fromIntegral m)

main :: IO ()
main = runGameOfLife

--   let helpTxt =
--         "Usage:\n\
--                 \    life [WIDTH HEIGHT] [SCALE]\n\
--                 \ * WIDTH - number of cells horizontally (default 100)\n\
--                 \ * HEIGHT - number of cells vertically (default 70)\n\
--                 \ * SCALE - scaling factor, or how many pixels one cell should take on a screen\n"
--   (_progName, args) <- getArgsAndInitialize
--   when (args == ["--help"]) $ putStrLn helpTxt >> exitSuccess
--   (m, n, s) <- case fmap readMaybe args of
--     [Just m, Just n, Just s]
--       | m > 0 && n > 0 && s > 0 -> return (m, n, s)
--     [Just m, Just n]
--       | m > 0 && n > 0 -> return (m, n, 10)
--     [] -> return (100, 70, 10)
--     _ -> do
--       putStrLn "Invalid arguments."
--       putStrLn helpTxt
--       exitWith $ ExitFailure 1
--   _w <- createGloss "Game of Life"
--   startGameOfLife (Sz2 m n) s
--   mainLoop


-- startGameOfLife :: Sz2 -> Int -> IO ()
-- startGameOfLife sz s = do
--   rowAlignment Unpack $= 1
--   let iLife = initLife sz inf2
--       wSz = size (pixelGrid s iLife)
--   G.glossSize $= sizeFromSz2 wSz
--   mArr <- new wSz
--   displayCallback $= clear [ColorBuffer]
--   drawLife s mArr iLife
--   runGameOfLife s mArr iLife


-- drawLife :: Int -> MArray RealWorld S Ix2 Word8 -> Array S Ix2 Word8 -> IO ()
-- drawLife s mArr arr = do
--   computeInto mArr $ pixelGrid s arr
--   A.withPtr mArr $ \ptr ->
--     drawPixels (sizeFromSz2 (msize mArr)) (PixelData Luminance UnsignedByte ptr)


-- drawLifeStep :: Int -> MArray RealWorld S Ix2 Word8 -> Array D Ix2 (Word8, Word8) -> IO ()
-- drawLifeStep s mArr arr = do
--   imapM_ updateCellLife arr
--   A.withPtr mArr $ \ptr ->
--     drawPixels (sizeFromSz2 (msize mArr)) (PixelData Luminance UnsignedByte ptr)
--   where
--     k = s + 1
--     updateCellLife (i :. j) (prev, next) =
--       when (prev /= next) $ do
--         let ixArr = makeArrayR D Seq (Sz2 s s) $ \(jc :. ic) -> (1 + jc + j * k) :. (1 + ic + i * k)
--             nVal = (1 - next) * 255
--         A.forM_ ixArr $ \ix -> write mArr ix nVal

-- runGameOfLife :: Int -> MArray RealWorld S Ix2 Word8 -> Array S Ix2 Word8 -> IO ()
-- runGameOfLife s mArr = go
--   where
--     go arr = do
--       let nextLife = life arr
--       drawLifeStep s mArr $ A.zip arr nextLife
--       flush
--       addTimerCallback 10 $ go nextLife
