module Model.Player where

import Model.Board ( Pos, Board , Ball(..), Color(..),genRandBall)
import System.Random -- (Random(randomRIO))
import GHC.Float (int2Float)
import Data.List (sort)
import qualified Data.Set as S

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------



angleList :: [(Int, Int)]
angleList = [(-3,1),(-2,1),(-3,2),(-1,1),
              (-2,3),(-1,2),(-1,3),
              (0,1),
              (1,3),(1,2),(2,3),(1,1),
              (3,2),(2,1),(3,1)]




-- >>> a
-- (-3,1)


data Player = Player
  { angle  :: Int
  , ballNum :: Int
  , ball :: Ball
  }

init :: Player
init = Player 7 10 (genRandBall 1001)




left :: Player -> Player
left p = p { angle = max (angle p - 1) 0}

right :: Player -> Player
right p = p { angle = min (angle p + 1) 14}

getPlayer :: Player -> ((Int,Int), Ball)
getPlayer p = ( angleList !! angle p, ball p)

isPlayerFinished :: Player -> Bool
isPlayerFinished p = ball p == Ball EMPTY

nextPlayer :: Player -> Player
nextPlayer p = case ballNum p of
                0 -> p
                1 -> p {ballNum = 0, ball = Ball EMPTY }
                _ -> p {ballNum = ballNum p - 1, ball = genRandBall (ballNum p)}

