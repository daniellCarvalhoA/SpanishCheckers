module GoBack (goBack) where 

import Graphics.Gloss.Data.Color 
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Display 


square :: Float -> Picture 
square f = color (greyN 0.5) $ rectangleSolid f f 

triangle :: Color -> Float -> Picture 
triangle c f = color c
           $ translate (thickness / 1.5) (- (thickness / 1.5))
           $ rotate 315
           $ polygon [(0,0),(0, - side),(tip / 1.3 ,( -side) / 2)] 
    where        
        tip = sqrt (side ^ 2 - ( side / 2) ^2)
        thickness = f / 4 - f / 10 
        side      = 1.5 * thickness 

goBack :: Color -> Float -> Picture
goBack c f = pictures [ square f
                      , color c
                      $ thickArc 0 315 radius thickness
                      , triangle c f
                      ]
    where radius = f / 4 
          thickness = f / 10

-- display  = displayIO win black (return $ goBack 100) (\_ -> return ())  
--     where win = InWindow "" (400, 400) (0, 0)