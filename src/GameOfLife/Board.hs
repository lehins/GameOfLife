{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GameOfLife.Board where

import Control.Monad
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
  }


lifeRules :: Word8 -> Word8 -> Word8
lifeRules 0 3 = 1
lifeRules 1 2 = 1
lifeRules 1 3 = 1
lifeRules _ _ = 0
{-# INLINE lifeRules #-}

lifeStencil :: Stencil Ix2 Word8 Word8
lifeStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ f ->
  lifeRules <$> f (0 :. 0) <*> (f (-1 :. -1) + f (-1 :. 0) + f (-1 :. 1) +
                                f ( 0 :. -1) +               f ( 0 :. 1) +
                                f ( 1 :. -1) + f ( 1 :. 0) + f ( 1 :. 1))
{-# INLINE lifeStencil #-}

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
{-# INLINE lifeStep #-}

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


initLifeBoard :: FullBoard -> Ix2 -> Sz2 -> LifeBoard
initLifeBoard board boardStart boardSize =
  LifeBoard
    { visibleStart = boardStart
    , visibleSize = boardSize
    , fullBoard = board
    , lifePaused = False
    }

lifeBoardStep :: MonadIO m => LifeBoard -> m LifeBoard
lifeBoardStep lifeBoard@LifeBoard {fullBoard, lifePaused}
  | lifePaused = pure lifeBoard
  | otherwise = do
    nextBoard <- lifeStep fullBoard
    pure lifeBoard {fullBoard = nextBoard}
{-# INLINE lifeBoardStep #-}

getBoardSize :: Monad m => LifeBoard -> m (Sz Ix2)
getBoardSize LifeBoard {..} = do
  pure $ msize $ lifeCurrent fullBoard
{-# INLINE getBoardSize #-}


mapAlive :: MonadIO m => (Ix2 -> a) -> LifeBoard -> m [a]
mapAlive f LifeBoard {..} =
  liftIO $ do
    cur <- unsafeFreeze Par (lifeCurrent fullBoard)
    curVisible <- extractM visibleStart visibleSize cur
    pure $!
      A.stoList $!
      simapMaybe (\ix e -> guard (e == 1) >> pure (f ix)) curVisible
{-# INLINE mapAlive #-}
