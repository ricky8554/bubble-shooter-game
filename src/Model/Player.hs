module Model.Player where

import Model.Board ( Pos, Board , Ball(..), Color(..),genRandBall)
import System.Random -- (Random(randomRIO))

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player
  { angle  :: Int
  , ballNum :: Int
  , ball :: Ball
  }

init :: Player
init = Player 90 1000 (genRandBall 1001)

left :: Player -> Player
left p = p { angle = min (angle p + 1) 170}

right :: Player -> Player
right p = p { angle = max (angle p - 1) 10}

getPlayer :: Player -> (Int, Ball)
getPlayer p = (angle p, ball p)

nextPlayer :: Player -> Player
nextPlayer p = case ballNum p of
                0 -> p
                1 -> p {ballNum = 0, ball = Ball EMPTY }
                _ -> p {ballNum = ballNum p - 1, ball = genRandBall (ballNum p)}