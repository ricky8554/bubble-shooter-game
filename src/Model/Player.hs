module Model.Player where

import Model.Board ( Pos, Board , Ball(..), Color(..), getExistBall)
import System.Random
import GHC.Float (int2Float)
import Data.List (sort)
import qualified Data.Set as S

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------
data Player = Player
  { angle  :: Int
  , ballNum :: Int
  , ball :: Ball
  }
  deriving (Eq)

-- >>> ((Player 7 12 (Ball YELLOW)) == (Player 7 12 (Ball YELLOW)))
-- True

angleList :: [(Int, Int)]
angleList = [(-3,1),(-2,1),(-3,2),(-1,1),
              (-2,3),(-1,2),(-1,3),
              (0,1),
              (1,3),(1,2),(2,3),(1,1),
              (3,2),(2,1),(3,1)]

realAngleList :: [Float]
realAngleList = [161.57, 153.43, 146.31, 135,
                123.7, 116.57, 108.43,
                90,
                71.57, 63.43, 56.3, 45,
                33.69, 26.57, 18.43]

init :: Board -> Player
init b = Player 7 12 (getExistBall b 1001)

left :: Player -> Player
left p = p { angle = max (angle p - 1) 0}

right :: Player -> Player
right p = p { angle = min (angle p + 1) 14}

getPlayer :: Player -> ((Int,Int), Ball)
getPlayer p = ( angleList !! angle p, ball p)

isPlayerFinished :: Player -> Bool
isPlayerFinished p = ball p == Ball EMPTY

nextPlayer :: Player -> Board -> Player
nextPlayer p b = case ballNum p of
                0 -> p {ballNum = 0, ball = Ball EMPTY }
                _ -> p {ballNum = ballNum p - 1, ball = getExistBall b (ballNum p)}
