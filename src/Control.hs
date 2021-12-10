module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Model.Score
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
import Model.FlyingBall
-- import Model.Player 

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS (play s)
  -- T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    s)
  -- T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt (s {psResult = Fail})
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Player  -> Player) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { ps = f (ps s) }



-------------------------------------------------------------------------------
-- play :: XO -> PlayState -> IO (Result Board)
-- -------------------------------------------------------------------------------
-- play xo s = put (psBoard s) xo <$> getPos xo s 

-- getPos :: XO -> PlayState -> IO Pos
-- getPos xo s = getStrategy xo s (psPos s) (psBoard s) xo

-- getStrategy :: XO -> PlayState -> Strategy 
-- getStrategy _ s = plStrat (ps s)

-- -------------------------------------------------------------------------------
-- nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-- -------------------------------------------------------------------------------

play :: PlayState -> PlayState
play s = if hasFlyingBall fb then s else s {ps = p, psFlying = fb'} 
  where
    p = nextPlayer (ps s) (psBoard s)
    fb = psFlying s
    fb' = setFlyingBall (getPlayer (ps s))

    


nextS :: PlayState -> EventM n (Next PlayState)
nextS s | failBoard b = halt (s {psResult = Fail})
        | hasFlyingBall (psFlying s) = Brick.continue s'
        | isBoardFinished b = nextBoard s'
        | isPlayerFinished (ps s) = halt s
        | otherwise = Brick.continue s' 
  where
    b = psBoard s
    fb' = nextFlyingBall (psFlying s)
    (b1,b') = updateBoard (getFlyingBall fb') b
    s' = s {psFlying = if b1 then setFlyingBall ((0,0),Ball EMPTY) else fb', psBoard = b'}    

nextBoard :: PlayState -> EventM n (Next PlayState)
nextBoard s = case res' of
                Win num -> halt (s {psResult = Win num}) 
                _       -> Brick.continue s' 
  where 
    sc'  = add (psScore s) (ballNum (ps s)) 
    res' = winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = psBoardList s (scG sc') -- clear the board
             , psFlying = Model.FlyingBall.init
             , ps       = Model.Player.init (psBoardList s (scG sc'))
             }