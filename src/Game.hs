module Game where

import Data.Word (Word32, Word8)
import Data.List.NonEmpty (NonEmpty)
import Data.Bits ( Bits((.|.), shiftL, complement, shift) )

data Game = Game {
    turn  :: Assoc
  , board :: Board
}

initialGame :: Game 
initialGame = Game (Human, White) initialboard


type Assoc = (Turn, Color)

data Turn = Human | Computer

data Size   = Pawn | King 

data Status = OnGoing | GameOver 

data Color  = White | Black 

data Board = Board {
    emptys :: Word32 
  , ups    :: Word32
  , kups   :: Word32
  , downs  :: Word32
  , kdowns :: Word32
} deriving Show

data Cache = Cache {
    pawn :: Word32 
  , king :: Word32
}

data ComputerMove = ComputerMove {
    start :: Word8 
  , cache :: Cache 
  , path  :: Word32 
  , end   :: Word8   
}

data Point = Point {
    cell :: Word8 
  , eat  :: Either Word8 Word8
}

type Path = NonEmpty Point

data Memoize = Memoize {
    pawns :: Word8 
  , kings :: Word8
}

data HumanMove = Cons {
    root   :: Word8 
  , forest :: Path
}



----------------------------------
---------------------------------

initialboard :: Board 
initialboard = Board empties' ups' kups' downs' kdowns' 
    where 
        ups'     = 0xfff 
        downs'   = ups' `shift` 20
        kups'    = 0
        kdowns'  = 0 
        empties' = complement $ ups' .|. downs' 

