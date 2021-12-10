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

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS (play s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt (s {psResult = Fail})
  _                               -> Brick.continue s

-------------------------------------------------------------------------------
move :: (Player  -> Player) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { ps = f (ps s) }

play :: PlayState -> PlayState
play s = if hasFlyingBall fb then s else s {ps = p', psFlying = fb'} 
  where
    p = ps s
    p' = p {Model.Player.ball = Ball EMPTY}
    fb = psFlying s
    fb' = setFlyingBall (getPlayer (ps s))

nextS :: PlayState -> EventM n (Next PlayState)
nextS s | hasFlyingBall (psFlying s) = Brick.continue s'
        | failBoard b = halt (s {psResult = Fail})
        | isBoardFinished b = nextBoard s'
        | isPlayerFinished (ps s) = halt (s {psResult = Fail})
        | otherwise = Brick.continue s' 
  where
    b = psBoard s
    fb' = nextFlyingBall (psFlying s)
    (b1, b') = updateBoard (getFlyingBall fb') b
    p = nextPlayer (ps s) b'
    s'  | b1        = s {ps = p, psFlying = setFlyingBall ((0,0),Ball EMPTY), psBoard = b'}
        | otherwise = s {psFlying = fb', psBoard = b'}    

nextBoard :: PlayState -> EventM n (Next PlayState)
nextBoard s = case res' of
                Win num -> halt (s {psResult = Win num}) 
                _       -> Brick.continue s' 
  where 
    sc'  = add (psScore s) (ballNum (ps s) + 1)
    res' = winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = psBoardList s (scG sc') -- clear the board
             , psFlying = Model.FlyingBall.init
             , ps       = Model.Player.init (psBoardList s (scG sc'))
             }