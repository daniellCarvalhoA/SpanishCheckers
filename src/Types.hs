{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RecordWildCards        #-}

module Types (
    Turn(..)
  , Size(..)
  , Direction(..)
  , AMove(..)
  , HMove(..)
  , Cache(..)
  , Point(..)
  , CMoves(..)
  , HMoves(..)  
  , IMove(..)
  , Result(..)
  , Color(..)
  , Forest
  , Move
  , CSimpleMove
  , HSimpleMove
  , otherColor
  , changeturn
  , append
  , consA
  , consH
  , alloweddirections
  ) where 

import Data.Bits           (Bits(..), FiniteBits(..))
import Data.Word           (Word64) 
import Data.List.NonEmpty  (NonEmpty(..), (<|))
import Control.Monad.Extra (ifM)
import Test.QuickCheck hiding (Result(..))

data Color = White | Black 
  deriving (Eq,Show) 

otherColor :: Color -> Color 
otherColor White = Black 
otherColor Black = White

data Result = Loss | Tie 
  deriving (Eq,Show) 

data Turn = Human | Computer
  deriving (Eq,Show)

instance Arbitrary Turn where
  arbitrary = ifM arbitrary (return Human) (return Computer)

changeturn :: Turn -> Turn 
changeturn Human    = Computer 
changeturn Computer = Human

data Size = Pawn | King 
  deriving (Eq, Show) 

------------------
-- Move Directions 

data Direction = NorthEast | NorthWest | SouthWest | SouthEast
  deriving (Show, Bounded, Eq, Enum)

class (Eq a, Bounded a, Enum a) => Rotate a where
  next :: a -> a
  before :: a -> a

instance Rotate Direction where
  next   a | a == maxBound = minBound
           | otherwise     = succ a
  before a | a == minBound = maxBound
           | otherwise     = pred a

alloweddirections :: Direction -> [Direction]
alloweddirections dir = [before dir, dir, next dir]

--------------------------
-- SimpleMove Types

type CSimpleMove  = (Size, Word64) 
type HSimpleMove  = (Size, (Word64, Word64))
type CSimpleMove' = Word64 



--------------------------
-- JumpMove Types 

data Cache = Cache {
    pawn :: !Word64
  , king :: !Word64
  } deriving Show 

instance Eq Cache where
  (Cache p1 k1) == (Cache p2 k2) = popCount p1 == popCount p2 && popCount k1 == popCount k2 

instance Ord Cache where
  (Cache p1 k1) <= (Cache p2 k2) 
    | popCount (p1 .|. k1) == popCount (p2 .|. k2) = popCount k1 <= popCount k2 
    | otherwise = popCount (p1 .|. k1) < popCount (p2 .|. k2)

instance Semigroup Cache where
  (Cache p1 k1) <> (Cache p2 k2) = Cache (p1 .|. p2) (k1 .|. k2) 

instance Monoid Cache where
  mempty = Cache 0 0 

append :: (Cache, [a]) -> (Cache, [a]) -> (Cache, [a])
append (c1,m1) (c2,m2)
  | c1 > c2   = (c1,m1) 
  | c2 > c1   = (c2,m2)
  | otherwise = (c1, m1 ++ m2)

---------------------------
-- Computer Jump Move 

data AMove = AMove {
    size  :: !Size 
  , path  :: !Word64
  , cache :: !Cache
  } deriving (Eq,Show)


consA :: Cache -> (Cache, [AMove]) -> (Cache, [AMove])
consA c (c', mvs) = (c <> c', consA' c <$> mvs)

consA' :: Cache -> AMove -> AMove
consA' c AMove{..} = AMove size path (c <> cache)

------------------------
-- The difference between these types is due to the need, in the case of human moves, 
-- to record intermediate steps. This es not necessary for computer moves. In the latter,
-- only the initial and final positions are needed.

-------------------------
-- Human Jump Moves 

data Point = Point {
    cell :: !Word64
  , eat  :: !Cache
  } deriving (Eq, Show)

type Forest = NonEmpty Point 

data HMove = HMove {
    siz    :: !Size
  , root   :: !Word64
  , forest :: !Forest 
  } deriving (Eq,Show) 

consH :: Cache -> Word64 -> (Cache, [Forest]) -> (Cache, [Forest])
consH eat x (c, frst) = (eat <> c, (Point x eat <|) <$> frst)

-------------------- 
-- All Move Type 

data CMoves = SCM (NonEmpty CSimpleMove)
            | JCM (NonEmpty AMove)
  deriving (Eq,Show) 

data HMoves = SHM (NonEmpty HSimpleMove)
            | JHM (NonEmpty HMove)
  deriving(Eq,Show) 

type Move = Either CSimpleMove AMove

type Begin = Word64
type End   = Word64 

data IMove = Start HMoves
           | MidSimple Size Begin (NonEmpty End)
           | MidJump Size Begin End Cache (NonEmpty Forest)
  deriving (Eq, Show) 

-- import Data.Word           (Word8, Word32)
-- import Data.List.NonEmpty  (NonEmpty, (<|))
-- import Data.Bits           (Bits(..))
-- import Control.Monad.Extra (ifM )
-- import Test.QuickCheck     (Arbitrary (arbitrary))

-- data HC = H | C

-- data Turn   = Human   | Computer deriving (Eq, Show)
-- data Size   = Pawn    | King     deriving (Eq, Show)
-- data Color  = White   | Black    deriving (Eq, Show)

-- instance Arbitrary Turn where
-- arbitrary = ifM arbitrary (pure Human) (pure Computer)

-- instance Arbitrary Color where
-- arbitrary = ifM arbitrary (pure White) (pure Black)

-- changeTurn :: Turn -> Turn
-- changeTurn Human    = Computer
-- changeTurn Computer = Human

-- otherColor :: Color -> Color
-- otherColor White = Black
-- otherColor Black = White




-----------------------------------------
-- ----- Cache gives the set of eaten pieces so far

-- data Cache = Cache {
-- pawn :: Word32
-- , king :: Word32
-- } deriving Show

-- instance Eq Cache where
-- (Cache p1 k1) == (Cache p2 k2) = popCount p1 == popCount p2 && popCount k1 == popCount k2

-- instance Ord Cache where
-- (Cache p1 k1) <= (Cache p2 k2)
-- | popCount p1 + popCount k1 == popCount p2 + popCount k2 = popCount k1 <= popCount k2
-- | otherwise = popCount p1 + popCount k1 < popCount p2 + popCount k2

-- instance Semigroup Cache where
-- (Cache p1 k1) <> (Cache p2 k2) = Cache (p1 .|. p2) (k1 .|. k2)

-- instance Monoid Cache where
-- mempty = Cache 0 0

-- cachePawn :: Word8 -> Cache
-- cachePawn i = Cache (bit j) 0 where j = fromIntegral i

-- cacheKing :: Word8 -> Cache
-- cacheKing i = Cache 0 (bit j) where j = fromIntegral i

------------
-- Directions

-- data Direction = NorthEast | NorthWest | SouthWest | SouthEast
-- deriving (Show, Bounded, Eq, Enum)

-- class (Eq a, Bounded a, Enum a) => Rotate a where
-- next :: a -> a
-- before :: a -> a
-- around :: a -> [a]

-- instance Rotate Direction where
-- next   a | a == maxBound = minBound
-- | otherwise     = succ a
-- before a | a == minBound = maxBound
-- | otherwise     = pred a
-- around a = [a .. maxBound] <> init [minBound .. a]

-- allowedDirections :: Direction -> [Direction]
-- allowedDirections dir = [before dir, dir, next dir]

-------------------------------
-- ------ Computer Move
-- --- # Computer and human have different move representations because the computer does not need
-- ----- directional information, which makes it faster for jump moves.
-- ----- Simple moves have the same representation for both computer and human moves

-- data ComputerMove = ComputerMove {
-- start :: Word8
-- , cache :: Cache
-- , end   :: Word8
-- } deriving Show

-- instance Eq ComputerMove where
-- m1 == m2 = cache m1 == cache m2

-- consM :: Cache -> Word8 -> (Cache, [ComputerMove]) -> (Cache, [ComputerMove])
-- consM c n (c', mvs) = (c <> c', consM' c n <$> mvs)

-- consM' :: Cache -> Word8 -> ComputerMove -> ComputerMove
-- consM' c n ComputerMove{..} = ComputerMove n (c <> cache) end

-------------------------------
---

-- type Eaten = Either Word8 Word8

-- data Point = Point {
-- cell :: Word8
-- , eat  :: Eaten
-- } deriving (Eq, Show)

-- type Path = NonEmpty Point

-- data Memoize = Memoize {
-- pawns :: Word8
-- , kings :: Word8
-- }

-- instance Eq Memoize where
-- m1 == m2 = pawns m1 == pawns m2 && kings m1 == kings m2

-- instance Ord Memoize where
-- (Memoize p1 k1) <= (Memoize p2 k2)
-- | p1 + k1 == p2 + k2 = k1 <= k2
-- | otherwise = p1 + k1 < p2 + k2

-- instance Semigroup Memoize where
-- (Memoize p1 k1) <> (Memoize p2 k2) = Memoize (p1 + p2) (k1 + k2)

-- instance Monoid Memoize where
-- mempty = Memoize 0 0

-- memoize :: Memoize -> Eaten -> Memoize
-- memoize Memoize{..} = \case
-- Left  _ -> Memoize (1 + pawns) kings
-- Right _ -> Memoize pawns (1 + kings)

-- mem :: Eaten -> Memoize
-- mem = \case
-- Left  _ -> Memoize 1 0
-- Right _ -> Memoize 0 1

-- cons :: Eaten -> Word8 -> (Memoize, [Path]) -> (Memoize, [Path])
-- cons eat x (m, p) = (memoize m eat, (Point x eat <|) <$> p)

-- cons' :: Eaten -> Word8 -> (Cache, [Path]) -> (Cache, [Path])
-- cons' eat x (c, p) = (addToCache c eat, (Point x eat <|) <$> p)

-- addToCache :: Cache -> Eaten -> Cache
-- addToCache c = \case
-- Left  w8 -> c <> cachePawn w8
-- Right w8 -> c <> cacheKing w8

-- data HumanMove = Cons {
-- root   :: Word8
-- , forest :: Path
-- } deriving (Eq, Show)
