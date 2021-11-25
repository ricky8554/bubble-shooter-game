module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board ( Pos(Pos, pRow, pCol), (!), Ball(..), Color(..), theight, bheight, bwidth )
import Graphics.Vty hiding (dim)


import System.Random (randomRIO)


-- sr :: [Integer]
-- sr = [1,2,3,4,5,6,7,8,9]

-- selectRandom :: [a] -> IO a
-- selectRandom xs = do
--   i <- randomRIO (0, length xs - 1)
--   return (sr !! i)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = 
  vLimit 40 $
    withBorderStyle unicode $
      -- borderWithLabel (str (header s)) $
        vTile [ mkRow s row | row <- [1..theight] ]

header :: PlayState -> String
header s = printf "" 

mkRow :: PlayState -> Int -> Widget n
mkRow s row = case row `mod` 2 of 
                1 -> hTile [ mkCell s row i | i <- [1..bwidth] ]
                _ -> padLeftRight 13 (hTile [ mkCell s row i | i <- [1..bwidth-1] ])

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c = mkCell' s r c 


-- withCursor :: Widget n -> Widget n
-- withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkBlock bmb)
  where 
    bmb      = psBoard s ! Pos r c
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkBlock :: Maybe Ball -> Widget n
mkBlock (Just (Ball RED)) = blockRed
mkBlock (Just (Ball BLUE)) = blockBlue
mkBlock (Just (Ball YELLOW)) = blockYellow
mkBlock (Just (Ball BLACK)) = blockBlack
mkBlock (Just (Ball GREEN)) = blockGreen
mkBlock _  = blockB

blockB, blockRed, blockBlue,blockYellow,blockBlack,blockGreen :: Widget n
blockB =      vBox (replicate 5 (str "     "))
blockRed =    vBox [ str "  •  "
                   , str "  •  "
                   , str "  •  "
                   , str "  •  "
                   , str "  •  "]
                   
blockBlue =   vBox [ str "•••••"
                   , str "    •"
                   , str "•••••"
                   , str "•    "
                   , str "•••••"]

blockYellow = vBox [ str "•••••"
                   , str "    •"
                   , str "•••••"
                   , str "    •"
                   , str "•••••"]

blockBlack = vBox  [ str "•   •"
                   , str "•   •"
                   , str "•••••"
                   , str "    •"
                   , str "    •"]

blockGreen = vBox  [ str "•••••"
                   , str "•    "
                   , str "•••••"
                   , str "    •"
                   , str "•••••"]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [b  | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [ b | b <- bs])
hTile _      = emptyWidget