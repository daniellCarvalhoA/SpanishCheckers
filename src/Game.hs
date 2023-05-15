{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Game where

import Data.Word          (Word32, Word8)
import Data.List.NonEmpty (NonEmpty(..),(<|))
import Data.Bits          ( Bits((.|.), shiftL, complement, popCount, bit) )

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

cellsPerRow :: Word8
cellsPerRow = 4

nofCells :: Word8
nofCells = 31 -- counting from 0

nofRows :: Word8
nofRows = 7  -- counting from 0

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

--------------
-- Directions

data Direction = NorthEast | NorthWest | SouthWest | SouthEast 
  deriving (Show, Bounded, Eq, Enum)

class (Eq a, Bounded a, Enum a) => Rotate a where
  next :: a -> a 
  before :: a -> a 
  around :: a -> [a]

instance Rotate Direction where 
  next :: Direction -> Direction
  next   a | a == maxBound = minBound
           | otherwise     = succ a 
  before :: Direction -> Direction
  before a | a == minBound = maxBound
           | otherwise     = pred a 
  around :: Direction -> [Direction]
  around a = [a .. maxBound] <> init [minBound .. a]

allowedDirections :: Direction -> [Direction]
allowedDirections dir = [before dir, dir, next dir]

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

instance Eq ComputerMove where 
  (==) :: ComputerMove -> ComputerMove -> Bool
  m1 == m2 = path m1 == path m2

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

instance Eq Memoize where 
  (==) :: Memoize -> Memoize -> Bool
  m1 == m2 = pawns m1 == pawns m2 && kings m1 == kings m2 

instance Ord Memoize where 
  (<=) :: Memoize -> Memoize -> Bool
  (Memoize p1 k1) <= (Memoize p2 k2) 
    | p1 + k1 == p2 + k2 = k1 <= k2 
    | otherwise = p1 + k1 < p2 + k2

instance Semigroup Memoize where
  (<>) :: Memoize -> Memoize -> Memoize
  (Memoize p1 k1) <> (Memoize p2 k2) = Memoize (p1 + p2) (k1 + k2)

instance Monoid Memoize where 
  mempty :: Memoize
  mempty = Memoize 0 0 

memoize :: Memoize -> Eaten -> Memoize
memoize Memoize{..} = \case 
  P _ -> Memoize (1 + pawns) kings 
  K _ -> Memoize pawns (1 + kings)

mem :: Eaten -> Memoize
mem = \case 
  P _ -> Memoize 1 0 
  K _ -> Memoize 0 1

cons :: Eaten -> Word8 -> (Memoize, [Path]) -> (Memoize, [Path])
cons eat x (m, p) = (memoize m eat, (Point x eat <|) <$> p)

cons' :: Eaten -> Word8 -> (Cache, [Path]) -> (Cache, [Path])
cons' eat x (c, p) = (addToCache c eat, (Point x eat <|) <$> p)

addToCache :: Cache -> Eaten -> Cache 
addToCache c = \case 
  P w8 -> c <> cachePawn w8 
  K w8 -> c <> cacheKing w8


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

