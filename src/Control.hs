module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
-- import Model.Player 

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  -- AppEvent Tick                   -> nextS s =<< liftIO (play O s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s 
  -- T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    s)
  -- T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
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

play s = s {ps = p, psBoard = b} 
  where
    b = nextBoard (getPlayer (ps s)) (psBoard s)
    p = nextPlayer (ps s)


nextS s = case ballNum (ps s) of
  0 -> halt s
  _ -> continue s' 
  where s' = play s



