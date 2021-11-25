module Model.Player where

import Model.Board ( Pos, Board , Ball(..), Color(..))
import System.Random -- (Random(randomRIO))

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player 
  { angle  :: Float  
  , ballNum :: Int 
  } 

init :: Player 
init = Player 0.0 1000

