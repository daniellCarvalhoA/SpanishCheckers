module ParseEvents where


import Data.Tuple.Extra (both) 
import Types
import Control.Applicative (Alternative(..))

-----------------------------------
-- Parse which color the user wishes to be

parseWhite :: CellSize -> PointInScreen -> Maybe Color
parseWhite cellsize (x,y)  
  | x > ( -7 * cellsize / 4) 
  , x < (- cellsize / 4) 
  , y > (- cellsize / 2) 
  , y < (  cellsize / 2) = Just White
  | otherwise            = Nothing

parseBlack :: CellSize -> PointInScreen -> Maybe Color 
parseBlack cellsize (x,y)  
  | x > (cellsize / 4)
  , x < (7 * cellsize / 4)
  , y > (- cellsize / 2)
  , y < (  cellsize / 2) = Just Black
  | otherwise            = Nothing

parseColor :: CellSize -> PointInScreen -> Maybe Color 
parseColor cellsize pt = parseWhite cellsize pt <|> parseBlack cellsize pt

-----------------------------------
-- Parse mid game events

data Event = CellClick Int   
           | UndoClick 
           | RedoClick 
           | AgainClick 

type CellSize = Float
type PointInScreen = (Float,Float) 

parseClick :: CellSize -> PointInScreen -> Maybe Event 
parseClick cellsize pointinscreen = 
  let playable p@(x,y) | x < (-4) || x > 3 || y < (-4) || y > 3 = Nothing 
                       | even $ x + y                           = Nothing 
                       | otherwise                              = Just p 
      normalize = both (floor . (/ cellsize))
  in CellClick . toboardnumber <$> (playable . normalize $ pointinscreen)

parseUndo :: CellSize -> PointInScreen -> Maybe Event 
parseUndo cellsize p 
  | x > 5.75 
  , x < 6.75
  , y > (- 0.5)
  , y < 0.5      = Just UndoClick 
  | otherwise    = Nothing 
  where (x,y) = both (/cellsize) p 
  
parseRedo :: CellSize -> PointInScreen -> Maybe Event 
parseRedo cellsize p 
  | x > 5.75 
  , x < 6.75
  , y > (- 2) 
  , y < (- 1) = Just RedoClick
  | otherwise = Nothing
  where (x,y) = both (/cellsize) p 

parseAgain :: CellSize -> PointInScreen -> Maybe Event 
parseAgain cellsize p 
  | x > (- 6.75)
  , x < (- 5.75) 
  , y > (- 0.6)
  , y < 0.4      = Just AgainClick 
  | otherwise    = Nothing 
  where (x,y) = both (/cellsize) p


parseEvent :: CellSize -> PointInScreen -> Maybe Event 
parseEvent cellsize pt = 
        parseClick cellsize pt 
    <|> parseUndo  cellsize pt 
    <|> parseAgain cellsize pt 
    <|> parseRedo  cellsize pt



toboardnumber :: (Int, Int) -> Int 
toboardnumber (x,y) = 
  let x' = x + 4 
      y' = y + 4 
  in x' `div` 2 + 4 * y' + y' `div` 2

