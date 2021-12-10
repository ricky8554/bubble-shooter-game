{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board (ResultB (..))

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scMax  :: Int  -- ^ total number of boards
  , scBall :: Int  -- ^ points for player  
  , scG    :: Int  -- ^ Finished games
  }
  deriving (Eq, Ord, Show)

init :: Int -> Score
init n = Score n 0 0

add :: Score -> Int -> Score
add sc num = sc { scBall = scBall sc + num, scG = scG sc + 1}

winner :: Score -> ResultB () 
winner sc@Score {..}
   | scG >= scMax     = Win scBall
   | otherwise        = Cont ()