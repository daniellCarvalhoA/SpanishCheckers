module Colors (
      nonCellColor
    , playingCellColor
    , whiteColor
    , blackColor
    , kingWhiteColor
    , kingBlackColor
    ) where 

import Graphics.Gloss.Data.Color

nonCellColor :: Color
nonCellColor = makeColorI 189 159 128 255

playingCellColor :: Color 
playingCellColor = makeColorI 94 68 43 255

whiteColor :: Color 
whiteColor = makeColorI 226 207 188 255

kingWhiteColor :: Color 
kingWhiteColor = dark whiteColor

blackColor :: Color
blackColor = makeColorI 30 17 3 255

kingBlackColor :: Color 
kingBlackColor = light blackColor

