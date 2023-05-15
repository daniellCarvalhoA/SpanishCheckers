{-# LANGUAGE InstanceSigs  #-}
module Game where

import Data.Word (Word32, Word8)
import Data.List.NonEmpty (NonEmpty)
import Data.Bits ( Bits((.|.), shiftL, complement, popCount, bit) )

data Game = Game {
    turn  :: Assoc
  , board :: Board
}

initialGame :: Game 
initialGame = Game (Human, White) initialboard

type Assoc = (Turn, Color)

data Turn   = Human   | Computer deriving (Eq, Show)
data Size   = Pawn    | King     deriving (Eq, Show)
data Status = OnGoing | GameOver deriving (Eq, Show)
data Color  = White   | Black    deriving (Eq, Show)

data Board = Board {
    emptys :: Word32 
  , ups    :: Word32
  , kups   :: Word32
  , downs  :: Word32
  , kdowns :: Word32
} deriving Show


-------------------------------------------
----- Cache gives the set of eaten pieces so far 

data Cache = Cache {
    pawn :: Word32 
  , king :: Word32
} deriving Show 

instance Eq Cache where 
  (==) :: Cache -> Cache -> Bool
  (Cache p1 k1) == (Cache p2 k2) = popCount p1 == popCount p2 && popCount k1 == popCount k2

instance Ord Cache where 
  (<=) :: Cache -> Cache -> Bool
  (Cache p1 k1) <= (Cache p2 k2)
    | popCount p1 + popCount k1 == popCount p2 + popCount k2 = popCount k1 <= popCount k2 
    | otherwise = popCount p1 + popCount k1 < popCount p2 + popCount k2 

instance Semigroup Cache where 
  (<>) :: Cache -> Cache -> Cache
  (Cache p1 k1) <> (Cache p2 k2) = Cache (p1 .|. p2) (k1 .|. k2)

instance Monoid Cache where 
  mempty :: Cache
  mempty = Cache 0 0

cachePawn :: Word8 -> Cache 
cachePawn i = Cache (bit j) 0 where j = fromIntegral i

cacheKing :: Word8 -> Cache 
cacheKing i = Cache 0 (bit j) where j = fromIntegral i 

---------------------------------
------ Computer Move 
--- # Computer and human have different move representations because the computer does not need 
----- directional information, which makes it faster for jump moves. 
----- Simple moves have the same representation

data ComputerMove = ComputerMove {
    start :: Word8 
  , cache :: Cache 
  , path  :: Word32 
  , end   :: Word8   
} deriving Show 

---------------------------------
-----

data Eaten = P Word8 
           | K Word8 
  deriving (Eq, Show)

data Point = Point {
    cell :: Word8 
  , eat  :: Eaten
} deriving Show 

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
        downs'   = ups' `shiftL` 20
        kups'    = 0
        kdowns'  = 0 
        empties' = complement $ ups' .|. downs' 

