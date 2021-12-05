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
import qualified Data.Set as S
import qualified Data.Matrix as MX
import Data.Maybe (isJust)
import System.Random

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------
data Color = RED | BLUE | YELLOW | BLACK | GREEN | EMPTY deriving (Eq, Ord, Show)
data Ball = Ball Color deriving (Eq,Show)

data Pos = Pos
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord,Show)

type Board = MX.Matrix Ball
type PosSet = S.Set Pos




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

allPos :: [Pos]
allPos = [ Pos i j | i <- [1..theight], j <- [1..bwidth] ]

-- emptyPositions :: Board -> [Pos]
-- emptyPositions board  = [ p | p <- positions, M.notMember p board]



init :: Board
init = MX.matrix theight bwidth $ uncurry randGenBalls

init1 :: Board
init1 = MX.matrix theight bwidth $ (\ (r, _y) -> Ball (colors !! (r `mod` colorNum)))

init2 :: Board
init2 = MX.matrix theight bwidth $ (\ (_x, c) -> Ball (colors !! (c `mod` colorNum)))

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

-- neighborExist :: Pos -> Board -> Bool
-- neighborExist pos board = or [Data.Maybe.isJust (board ! p) | p <- findNeighbor pos]

findNeighbor :: Pos -> [Pos]
findNeighbor (Pos r c) = if even r then pEven else pOdd
  where
    pOdd  = [Pos (r-1) c, Pos (r-1) (c-1), Pos r (c+1), Pos r (c-1), Pos (r+1) c, Pos (r+1) (c-1)]
    pEven = [Pos (r-1) c, Pos (r-1) (c+1), Pos r (c+1), Pos r (c-1), Pos (r+1) c, Pos (r+1) (c+1)]

insertBoard :: Pos -> Ball -> Board -> Board
insertBoard (Pos r c) ball board = case MX.safeSet ball (r, c) board of
                                      Just b  -> b
                                      _       -> board

setBoard :: Pos -> Ball -> Board -> Board
setBoard = insertBoard

setBoardEmpty :: Pos -> Board -> Board
setBoardEmpty pos board = setBoard pos (Ball EMPTY) board

dfsColor' :: Board -> Ball -> Pos -> PosSet -> PosSet
dfsColor' board ball pos set = if S.member pos set then set else newset
  where
    neighbor = filter (\p -> (board ! p) == Just ball ) (findNeighbor pos)
    newset = foldr next (S.insert pos set) neighbor
    next pos' set' = dfsColor' board ball pos' set'

dfsColor :: Board -> Maybe Ball -> Pos -> PosSet
dfsColor board (Just ball) pos = dfsColor' board ball pos S.empty 
dfsColor _ Nothing _ = S.empty

dfs' :: Board -> Pos -> PosSet -> PosSet
dfs' board pos set = if S.member pos set then set else newset
  where
    neighbor = filter (\p -> isJust (board ! p) ) (findNeighbor pos)
    newset = foldr next (S.insert pos set) neighbor
    next pos' set' = dfs' board pos' set'

dfs :: Board -> Pos -> PosSet -> (PosSet, [Pos])
dfs board pos set = if S.member pos set then (S.empty, []) else (s, l)
  where  
    s = dfs' board pos S.empty 
    l' = S.toList s
    l = if any attachWall l' then [] else l'

attachWall :: Pos -> Bool 
attachWall (Pos 1 _) = True 
attachWall (Pos x 1) = odd x
attachWall (Pos _ y) = y == bwidth 


removeFromBoard :: Pos -> Board -> Board
removeFromBoard pos board = if size > 2 then newBoard else board
  where
    dfsSet = dfsColor board (board ! pos) pos
    size = S.size dfsSet
    dfsList = S.toList dfsSet
    newBoard = foldr setBoardEmpty board dfsList

removeDetachBoard :: Board -> Board
removeDetachBoard board = newBoard
  where
    l = filter (\p -> isJust (board ! p)) allPos
    (_, rl) = foldr dfsD (S.empty,[]) l
    dfsD pos (s,l) = slu (dfs board pos s) (s,l)
    slu (s1,l1) (s2,l2) = (S.union s1 s2, l1 ++ l2)
    newBoard = foldr setBoardEmpty board rl



-- >>> init1

-- >>> removeFromBoard (Pos 1 1) init1

-- >>> removeFromBoard (Pos 2 1) init1


-- >>> a = (setBoardEmpty (Pos 1 3) (setBoardEmpty (Pos 3 3) (setBoardEmpty (Pos 3 2) (setBoardEmpty (Pos 1 2) (removeDetachBoard (removeFromBoard (Pos 2 1) (setBoard (Pos 2 2) (Ball RED) (removeFromBoard (Pos 2 1) init1))))))))  
-- >>> removeDetachBoard a



-- >>> removeFromBoard (Pos 2 1) init2

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

