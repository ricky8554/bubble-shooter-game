{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player
import qualified Model.FlyingBall as FlyingBall

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { ps       :: Player.Player   -- ^ player X info
  , psBoard  :: Board.Board     -- ^ current board
  , psFlying :: FlyingBall.FlyingBall
  , psScore  :: Score.Score
  , psResult :: Board.ResultB ()
  , psBoardList :: Int -> Board.Board
  }

init :: Int -> PlayState
init n = PS 
  { ps       = Player.init (Board.init 0)
  , psBoard  = Board.init 0
  , psFlying = FlyingBall.init
  , psScore  = Score.init n
  , psResult = Board.Cont ()
  , psBoardList = Board.init 
  }