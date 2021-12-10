module View (view,atrMap) where

import Brick
    ( attrMap,
      on,
      emptyWidget,
      hBox,
      padLeftRight,
      str,
      vBox,
      withBorderStyle,
      AttrMap,
      AttrName,
      Widget, attrName, withAttr, Location (Location), hLimitPercent )
import Brick.Widgets.Center (center, hCenterLayer)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, border)

import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Text.Printf (printf)

import Model
import Model.Board ( Pos(Pos, pRow, pCol), (!), Ball(..), Color(..), theight, bheight, bwidth )
import Graphics.Vty hiding (dim)


import System.Random (randomRIO)
import qualified Graphics.Vty as V
import Model.FlyingBall
import Model.Player
import Model.Score
import GHC.Float (int2Float, float2Int)
import Brick.Widgets.Core (setAvailableSize, translateBy)
import Brick.Types (Location(loc))

-- sr :: [Integer]
-- sr = [1,2,3,4,5,6,7,8,9]

-- selectRandom :: [a] -> IO a
-- selectRandom xs = do
--   i <- randomRIO (0, length xs - 1)
--   return (sr !! i)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [ hCenterLayer (angleS s),view' s ]

angleS :: PlayState -> Widget n
angleS s = if flfb == (Ball EMPTY) then translateBy ll (mkfb' mkfb) else emptyWidget
  where 
    ((dx,dy),ball) = getPlayer (ps s)
    mkfb' f =  (vBox ( reverse  [ f y  | y <- [1..ss]]))
    mkfb y = (hBox [ mkBlock' (get x y) | x <- [-ssh+1..ssh]])
    valid x y = if dx == 0 
                then x == 0
                else x `mod` dx == 0 && y `mod` dy == 0 && x `div` dx == y `div` dy
    get x y = if valid x y then b2 else (if (inrange x y) then b3 else b1)
    b1 = Just (Ball EMPTY ) 
    b2 = Just (Ball SPECIAL)
    b3 = Just ball
    (FlyingBall _ _ _ _ flfb) = psFlying s
    inrange x y = abs x < 12 && y < 11
    ss = 24
    ssh = 24
    ll = (Location (0,57))

view' :: PlayState -> Widget String
view' s =
    center $ withBorderStyle unicodeBold $
        borderWithLabel (str (header s)) $
        vBox [ mkRow s row | row <- [1..theight] ]

header :: PlayState -> String
header s = printf (" Stage:" ++ show ((scG sc) + 1) ++ ", Score:" ++ show (scBall sc) ++ ", Balls:" ++ show (ballNum p) ++ ", Degree:" ++ show (realAngleList !! (angle p)) ++ " ")
  where
    sc = psScore s
    p  = ps s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = case even row || row == theight  of
                True -> hBox [ mkCell s row i | i <- [1..bwidth+1]]
                _ -> hBox [ mkCell s row i | i <- [1..bwidth] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c = mkCell' s r c

posFB :: PlayState -> Int -> Int -> (Int, Int, Int)
posFB s r c =
  case (flfb == Ball EMPTY,
        inRange x y,
        inRange (x + 0.95) y,
        inRange x  (y + 1),
        inRange (x + 0.95) (y + 1)) of
    (True, _, _, _, _) -> (0,0,0)
    (_, True, _, _, _) -> (1,wrap (x - mc), wrap y)
    (_, _, True, _, _) -> (2,wrap (x + 1 - mc), wrap y)
    (_, _, _, True, _) -> (3,wrap (x - mc) , wrap (y + 1))
    (_, _, _, _, True) -> (4,wrap (x + 1 - mc), wrap (y + 1))
    _ -> (0,0,0)
  where
    (FlyingBall x y _ _ flfb) = psFlying s
    lr = int2Float r
    lc = int2Float c - mc
    rr = int2Float r + 1
    rc = int2Float c + 1 - mc
    mc = if even r then 0.5 else 0
    fa a= int2Float (floor a)
    wrap a = float2Int ((a - fa a) * bsf)
    inRange c' r' = (lr <= r' && r' < rr) && (lc <= c' && c' < rc)
-- withCursor :: Widget n -> Widget n
-- withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c
  | hasFB > 0 = (mkFBCell hasFB r c y' x' bmb''')
  | r == theight && c == (bwidth `div` 2 + 1) =  center (mkCenterBall s)
  | odd r = center (mkBlock bmb)
  | c == 1 || c == (bwidth + 1) =  mkBlock (Just (Ball HALF))
  | otherwise =  center (mkBlock bmb')
  where
    bmb      = psBoard s ! Pos r c
    bmb'     = psBoard s ! Pos r (c-1)
    bmb'''   = Just flfb
    (hasFB, x', y')   = posFB s r c
    (FlyingBall _ _ _ _ flfb) = psFlying s

mkCenterBall :: PlayState -> Widget n
mkCenterBall s = mkBlock'' bs bs (Just ball)
  where
    ((_,_),ball) = getPlayer (ps s)
    
mkFBCell :: Int -> Int -> Int -> Int -> Int -> Maybe Ball -> Widget n
mkFBCell v r c r' c' ball
  | v == 1 = mkfb' mkfb1
  | v == 2 = mkfb' mkfb2
  | v == 3 = mkfb' mkfb3
  | v == 4 = mkfb' mkfb4
  where
    ep  = Nothing
    mkfb' f = center1 (vBox [ f y  | y <- [1..bs]])
    mkfb1 y = center1 (hBox [ mkBlock' (if x > c' && y > r' `div` 2 then ball else ep) | x <- [st..end]])
    mkfb2 y = center1 (hBox [ mkBlock' (if x < c' && y > r' `div` 2 then ball else ep) | x <- [st..end]])
    mkfb3 y = center1 (hBox [ mkBlock' (if x > c' && y < r' `div` 2 then ball else ep) | x <- [st..end]])
    mkfb4 y = center1 (hBox [ mkBlock' (if x < c' && y < r' `div` 2 then ball else ep) | x <- [st..end]])
    -- end = if even r && c == 1 then bsh else bs
    -- end =  bsh else bs
    center1 cen = if even r && (c == 1 || c == bwidth + 1) then cen else center cen
    st = if even r && c == 1 then bsh else 1
    end = if even r && c == bwidth + 1 then bsh  else bs
mkFBCell _ _ _ _ _ _ = mkBlock (Just (Ball EMPTY))

mkBlock :: Maybe Ball -> Widget n
mkBlock (Just (Ball RED)) = border (withAttr redAtr blockB)
mkBlock (Just (Ball BLUE)) = border (withAttr blueAtr blockB)
mkBlock (Just (Ball YELLOW)) = border (withAttr yellowAtr blockB)
mkBlock (Just (Ball BLACK)) = border (withAttr blackAtr blockB)
mkBlock (Just (Ball GREEN)) = border (withAttr greenAtr blockB)
mkBlock (Just (Ball HALF)) = blockHalf
mkBlock _  = blockB

mkBlock' ::  Maybe Ball -> Widget n
mkBlock'  (Just (Ball RED)) =  (withAttr redAtr blockC)
mkBlock'  (Just (Ball BLUE)) =  (withAttr blueAtr blockC)
mkBlock' (Just (Ball YELLOW)) =  (withAttr yellowAtr blockC)
mkBlock'  (Just (Ball BLACK)) =  (withAttr blackAtr blockC)
mkBlock' (Just (Ball GREEN)) =  (withAttr greenAtr blockC)
mkBlock' (Just (Ball SPECIAL)) =  (withAttr test1Atr blockC)
mkBlock'  _  =  blockC


mkBlock'' ::  Int -> Int -> Maybe Ball -> Widget n
mkBlock'' x y (Just (Ball RED)) =  (withAttr redAtr (block x y))
mkBlock'' x y (Just (Ball BLUE)) =  (withAttr blueAtr (block x y))
mkBlock'' x y (Just (Ball YELLOW)) =  (withAttr yellowAtr (block x y))
mkBlock'' x y (Just (Ball BLACK)) =  (withAttr blackAtr (block x y))
mkBlock'' x y (Just (Ball GREEN)) =  (withAttr greenAtr (block x y))
mkBlock'' x y (Just (Ball SPECIAL)) =  (withAttr test1Atr (block x y))
mkBlock''  x y _  =  (block x y)

bs :: Int
bs = 24
bsh :: Int
bsh = bs `div` 2
bsf :: Float
bsf = int2Float bs

blockB :: Widget n
blockB = block bs bs
blockC :: Widget n
blockC = block 1 1

blockHalf :: Widget n
blockHalf = block bs bsh

block :: Int -> Int -> Widget n
block h w =  vBox (replicate h (str (replicate w ' ')))

redAtr, blueAtr, yellowAtr, blackAtr, greenAtr,testAtr,test1Atr :: AttrName
redAtr    = attrName "redAtr"
blueAtr   = attrName "blueAtr"
yellowAtr = attrName "yellowAtr"
blackAtr  = attrName "blackAtr"
greenAtr  = attrName "greenAtr"
testAtr  = attrName "testAtr"
test1Atr  = attrName "test1Atr"

atrMap :: AttrMap
atrMap = attrMap V.defAttr [
    (redAtr,   V.red  `on` V.red)
   ,(blueAtr,  V.blue `on` V.blue )
   ,(yellowAtr,V.yellow `on` V.yellow )
   ,(blackAtr,  V.white  `on` V.white )
   ,(greenAtr,  V.green  `on` V.green )
   ,(testAtr,  V.cyan `on` V.cyan )
   ,(test1Atr,  V.magenta  `on` V.magenta )
  ]