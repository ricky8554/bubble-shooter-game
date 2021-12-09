{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
-- import qualified Model.Score  as Score
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
  } 

init :: Int -> PlayState
init n = PS 
  { ps       = Player.init Board.init
  , psBoard  = Board.init
  , psFlying = FlyingBall.init 
  }


-- next :: PlayState -> PlayState
-- next s = case getBall (ps s) of

-- nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
-- nextBoard s res = Right s { psBoard = mempty } 
-- next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
-- next s Board.Retry     = Right s
-- next s (Board.Cont b') = Right (s { psBoard = b'})
-- next s res             = nextBoard s res 

