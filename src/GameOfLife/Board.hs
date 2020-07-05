{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GameOfLife.Board where

import Data.IORef
import Data.Int
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Word

type RW = RealWorld

data FullBoard =
  FullBoard
    { lifePrevious  :: !(MArray RW S Ix2 Word8)
    , lifeCurrent   :: !(MArray RW S Ix2 Word8)
    , lifeIteration :: !Int
    }

data LifeBoard =
  LifeBoard
  { visibleStart   :: !Ix2
  , visibleSize    :: !Sz2
  , fullBoard      :: !FullBoard
  , lifePaused     :: !Bool
  , lifeBoardDirty :: !(IORef Bool)
  }


lifeRules :: Word8 -> Word8 -> Word8
lifeRules 0 3 = 1
lifeRules 1 2 = 1
lifeRules 1 3 = 1
lifeRules _ _ = 0

lifeStencil :: Stencil Ix2 Word8 Word8
lifeStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ f ->
  lifeRules <$> f (0 :. 0) <*> (f (-1 :. -1) + f (-1 :. 0) + f (-1 :. 1) +
                                f ( 0 :. -1) +               f ( 0 :. 1) +
                                f ( 1 :. -1) + f ( 1 :. 0) + f ( 1 :. 1))

lifeStep :: MonadIO m => FullBoard -> m FullBoard
lifeStep FullBoard {..} =
  liftIO $ do
    cur <- unsafeFreeze Par lifeCurrent
    lifeStepM lifePrevious cur
    pure
      FullBoard
        { lifePrevious = lifeCurrent
        , lifeCurrent = lifePrevious
        , lifeIteration = lifeIteration + 1
        }

lifeStepM :: MonadIO m => MArray RW S Ix2 Word8 -> Array S Ix2 Word8 -> m ()
lifeStepM marr = computeInto marr . mapStencil Wrap lifeStencil

initLife :: MonadIO m => Sz2 -> Array S Ix2 Word8 -> m FullBoard
initLife sz arr =
  liftIO $ do
    let ix0 = liftIndex (`div` 2) (unSz (sz - size arr))
        lifeIteration = 0
    lifePrevious <- new sz
    lifeCurrent <-
      unsafeThaw =<<
      computeIO
        (insertWindow
           (makeArrayR D Par sz (const 0))
           (Window ix0 (size arr) (index' arr . subtract ix0) Nothing))
    pure FullBoard {..}


initLifeBoard :: MonadIO m => FullBoard -> Ix2 -> Sz2 -> m LifeBoard
initLifeBoard board boardStart boardSize = liftIO $ do
  lifeBoardDirty <- newIORef True
  pure LifeBoard
    { visibleStart = boardStart
    , visibleSize = boardSize
    , fullBoard = board
    , lifePaused = False
    , lifeBoardDirty = lifeBoardDirty
    }

lifeBoardStep :: MonadIO m => LifeBoard -> m LifeBoard
lifeBoardStep lifeBoard@LifeBoard {fullBoard, lifePaused}
  | lifePaused = pure lifeBoard
  | otherwise = do
    nextBoard <- lifeStep fullBoard
    pure lifeBoard {fullBoard = nextBoard}

getBoardSize :: Monad m => LifeBoard -> m (Sz Ix2)
getBoardSize LifeBoard {..} = do
  pure $ msize $ lifeCurrent fullBoard

extractVisibleChange :: MonadIO m => LifeBoard -> m (Array S Ix2 Int8)
extractVisibleChange LifeBoard {..} =
  liftIO $ do
    isDirty <- atomicModifyIORef' lifeBoardDirty (\e -> (True, e))
    if isDirty
      then do
        cur <- unsafeFreeze Par (lifeCurrent fullBoard)
        curVisible <- extractM visibleStart visibleSize cur
        computeIO $ A.map getDirtyChange curVisible
      else do
        prev <- unsafeFreeze Par (lifePrevious fullBoard)
        cur <- unsafeFreeze Par (lifeCurrent fullBoard)
        prevVisible <- extractM visibleStart visibleSize prev
        curVisible <- extractM visibleStart visibleSize cur
        computeIO $ A.zipWith getChange prevVisible curVisible
  where
    getDirtyChange c
      | c == 0 = -1
      | otherwise = 1
    getChange p c
      | p == c = 0
      | p > c = -1
      | otherwise = 1
