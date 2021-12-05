{-# LANGUAGE DeriveFunctor #-}
module Model.Board
  ( -- * Types
    Board,
    Ball (..),
    Color (..)
  -- , XO (..)
  , Pos (..)
  -- , Result (..)

    -- * Board API
  -- , dim
  , bheight
  , theight
  , bwidth
  , (!)
  , init
  , genRandBall
  -- , put
  -- , positions
  -- , emptyPositions
  -- , boardWinner
  -- , flipXO

  --   -- * Moves
  -- , up
  -- , down
  -- , left
  -- , right
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M
import qualified Data.Matrix as MX
import Data.Maybe (isJust)
import System.Random

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------
data Color = RED | BLUE | YELLOW | BLACK | GREEN | EMPTY deriving (Eq, Ord, Show)
data Ball = Ball Color deriving (Show)

data Pos = Pos
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

type Board = MX.Matrix Ball




(!) :: Board -> Pos -> Maybe Ball
board ! (Pos r c) = case MX.safeGet r c board of
                      Just (Ball EMPTY) -> Nothing
                      x -> x

bheight :: Int
bheight = 5

theight :: Int
theight = bheight * 1

bwidth :: Int
bwidth = 10

colorNum:: Int
colorNum = 5

colors :: [Color]
colors = [ RED, BLUE,  BLACK, YELLOW,GREEN]



-- emptyPositions :: Board -> [Pos]
-- emptyPositions board  = [ p | p <- positions, M.notMember p board]



init :: Board
init = MX.matrix theight bwidth $ uncurry randGenBalls


rvalues :: [Int]
rvalues = map fst $ scanl (\(_, gen) _ -> random gen) (random (mkStdGen 1)) $ repeat ()

randList :: [Int]
randList = map abs (take (bheight*bwidth*1000) rvalues)


randGenBalls :: Int -> Int -> Ball
randGenBalls r c =
  case (r `mod` 2, r > bheight, bwidth - c) of
    (0, _ ,0) -> Ball EMPTY
    (_, True  ,_) -> Ball EMPTY
    _ -> Ball (colors !! ((randList !! ((r-1)*bwidth + c - 1) ) `mod` colorNum ) )

genRandBall :: Int -> Ball
genRandBall n = Ball (colors !! (randList !! (bheight*bwidth + n) `mod` colorNum ) )

neighborExist :: Pos -> Board -> Bool
neighborExist pos board = or [Data.Maybe.isJust (board ! p) | p <- findNeighbor pos]

findNeighbor :: Pos -> [Pos]
findNeighbor (Pos r c) = map checkBoundry p
  where
    pEven = [Pos (r-1) c, Pos (r-1) (c-1), Pos r (c+1), Pos r (c-1), Pos (r+1) c, Pos (r+1) (c-1)]
    pOdd = [Pos (r-1) c, Pos (r-1) (c+1), Pos r (c+1), Pos r (c-1), Pos (r+1) c, Pos (r+1) (c+1)]
    p = if even r then pEven else pOdd
    checkBoundry :: Pos -> Pos
    checkBoundry (Pos r c) = let rt
                                   | r < 1 = 1
                                   | r > bheight = bheight
                                   | otherwise = r
                                 ct
                                   | c < 1 = 1
                                   | c > bwidth = bwidth
                                   | otherwise = c
                                 in Pos rt ct

insertBoard :: Pos -> Ball -> Board -> Board
insertBoard (Pos r c) ball board = case safeSet ball (r, c) board of
                                      Just b -> b
                                      otherwise -> board

setBoard :: Pos -> Ball -> Board -> Board
setBoard (Pos r c) ball = safeset ball (r, c)

removeFromBoard :: Pos -> Board -> Board
removeFromBoard pos board = if findNumSameNeighbor pos (board ! pos) board 0 >= 3
                              then remove pos (board ! pos) board
                              else board
  where
    remove pos1(Pos r c) ball board1 = foldl (\b p -> remove p ball b) newBoard sameNeighbors
      where
        sameNeighbors = foldl (\b p -> if board1 ! p == ball then p:b else b) [] (findNeighbor pos1)
        newBoard = foldl (\b p -> if b ! p == ball then setBoard p (Ball EMPTY) b else b) board1 sameNeighbors
    findNumSameNeighbor pos2(Pos r c) ball board2 num = foldl (\b p -> findSameNeighbor p ball board2 b) newNum sameNeighbors
      where
        sameNeighbors = foldl (\b p -> if board2 ! p == ball then p:b else b) [] (findNeighbor pos2)
        newNum = foldl (\b p -> if member p result then b else insert p b) num sameNeighbors

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------

-- data Result a 
--   = Draw 
--   | Win XO
--   | Retry 
--   | Cont a
--   deriving (Eq, Functor, Show)

-- put :: Board -> XO -> Pos -> Result Board
-- put board xo pos = case M.lookup pos board of 
--   Just _  -> Retry
--   Nothing -> result (M.insert pos xo board) 

-- result :: Board -> Result Board
-- result b 
--   | isFull b  = Draw
--   | wins b X  = Win  X 
--   | wins b O  = Win  O
--   | otherwise = Cont b

-- wins :: Board -> XO -> Bool
-- wins b xo = or [ winsPoss b xo ps | ps <- winPositions ]

-- winsPoss :: Board -> XO -> [Pos] -> Bool
-- winsPoss b xo ps = and [ b!p == Just xo | p <- ps ]

-- winPositions :: [[Pos]]
-- winPositions = rows ++ cols ++ diags 

-- rows, cols, diags :: [[Pos]]
-- rows  = [[Pos r c | c <- [1..dim]] | r <- [1..dim]]
-- cols  = [[Pos r c | r <- [1..dim]] | c <- [1..dim]]
-- diags = [[Pos i i | i <- [1..dim]], [Pos i (dim+1-i) | i <- [1..dim]]]

-- isFull :: Board -> Bool
-- isFull b = M.size b == dim * dim

-- -------------------------------------------------------------------------------
-- -- | Moves 
-- -------------------------------------------------------------------------------

-- up :: Pos -> Pos 
-- up p = p 
--   { pRow = max 1 (pRow p - 1) 
--   } 

-- down :: Pos -> Pos
-- down p = p 
--   { pRow = min dim (pRow p + 1) 
--   } 

-- left :: Pos -> Pos 
-- left p = p 
--   { pCol   = max 1 (pCol p - 1) 
--   } 

-- right :: Pos -> Pos 
-- right p = p 
--   { pCol = min dim (pCol p + 1) 
--   } 

-- boardWinner :: Result a -> Maybe XO
-- boardWinner (Win xo) = Just xo
-- boardWinner _        = Nothing

-- flipXO :: XO -> XO
-- flipXO X = O
-- flipXO O = X

