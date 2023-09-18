{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns    #-}

module JumpMoves   where

import Types              ( Turn(..)
                          , Size(..)
                          , AMove(..)
                          , HMove(..)
                          , Direction(..)
                          , Cache(..)
                          , Point(..)
                          , consA
                          , consH
                          , append
                          , alloweddirections
                          )
import Board              (Board(..), empties, lowerRow, upperRow)
import Data.Word          (Word64)
import Data.Bits          (FiniteBits(..), Bits(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List          (unfoldr, nub)
import Data.Foldable      (foldl')
import Control.Arrow      (second)
import Data.Function      ((&))
-----------------
-- General Utils 

leftUpEat :: Board -> Word64 -> (Cache, Word64)
leftUpEat b@Board{..} n
  | j .&. downpawns /= 0
  , i .&. es        /= 0 = (Cache j 0, i)
  | j .&. downkings /= 0
  , i .&. es        /= 0 = (Cache 0 j, i)
  | otherwise            = (Cache 0 0,0)
  where j  = n `shiftL` 4
        i  = j `shiftL` 4
        es = empties b

rightUpEat :: Board -> Word64 -> (Cache, Word64)
rightUpEat b@Board{..} n
  | j .&. downkings /= 0
  , i .&. es        /= 0 = (Cache 0 j, i)
  | j .&. downpawns /= 0
  , i .&. es        /= 0 = (Cache j 0, i)
  | otherwise            = (Cache 0 0,0)
  where j  = n `shiftL` 5
        i  = j `shiftL` 5
        es = empties b

leftDownEat :: Board -> Word64 -> (Cache, Word64)
leftDownEat b@Board{..} n
  | j .&. upkings /= 0
  , i .&. es      /= 0 = (Cache 0 j, i)
  | j .&. uppawns /= 0
  , i .&. es      /= 0 = (Cache j 0, i)
  | otherwise          = (Cache 0 0,0)
  where j  = n `shiftR` 5
        i  = j `shiftR` 5
        es = empties b

rightDownEat :: Board -> Word64 -> (Cache, Word64)
rightDownEat b@Board{..} n
  | j .&. upkings /= 0
  , i .&. es      /= 0  = (Cache 0 j, i)
  | j .&. uppawns /= 0
  , i .&. es      /= 0  = (Cache j 0, i)
  | otherwise           = (Cache 0 0, 0)
  where j  = n `shiftR` 4
        i  = j `shiftR` 4
        es = empties b

leftEat :: Board -> Word64 -> Turn -> (Cache, Word64)
leftEat board i = \case
  Human    -> leftUpEat   board i
  Computer -> leftDownEat board i

rightEat :: Board -> Word64 -> Turn -> (Cache, Word64)
rightEat board i = \case
  Human    -> rightUpEat   board i
  Computer -> rightDownEat board i

untilObstacle :: Turn -> (Word64 -> Word64) -> Word64 -> Board -> Maybe (Word64, Cache)
untilObstacle Human f n b@Board{..} = 
  case f n of
    0 -> Nothing
    i | i .&. downkings /= 0 -> Just (i, Cache 0 i)
      | i .&. downpawns /= 0 -> Just (i, Cache i 0)
      | i .&. es /= 0   -> untilObstacle Human f i b
      | otherwise       -> Nothing
  where es    = empties b
untilObstacle Computer f n b@Board{..} = 
  case f n of
    0 -> Nothing
    i | i .&. upkings /= 0 -> Just (i, Cache 0 i)
      | i .&. uppawns /= 0 -> Just (i, Cache i 0)
      | i .&. es /= 0   -> untilObstacle Computer f i b
      | otherwise       -> Nothing
  where es    = empties b

rightUpAdj, leftUpAdj, rightDownAdj, leftDownAdj :: Word64 -> Word64
rightUpAdj   = (`shiftL` 5)
leftUpAdj    = (`shiftL` 4)
rightDownAdj = (`shiftR` 4)
leftDownAdj  = (`shiftR` 5)

diagonal :: Cache -> Turn -> (Word64 -> Word64) -> Word64 -> Board -> ([Word64], Cache)
diagonal Cache{..} turn f n b =
  case untilObstacle turn f n b of
    Nothing     -> ([], mempty)
    Just (i, c) -> (,c) $
      unfoldr (\x -> case f x of
        0 -> Nothing
        j | j .&. (pawn .|. king) /= 0 -> Nothing
          | j .&. es /= 0              -> Just (j,j)
          | otherwise                  -> Nothing
              ) i
  where es = empties b

rudiagonal :: Cache -> Turn -> Word64 -> Board -> ([Word64], Cache)
rudiagonal cache turn = diagonal cache turn rightUpAdj

ludiagonal :: Cache -> Turn -> Word64 -> Board -> ([Word64], Cache)
ludiagonal cache turn = diagonal cache turn leftUpAdj

rddiagonal :: Cache -> Turn -> Word64 -> Board -> ([Word64], Cache)
rddiagonal cache turn = diagonal cache turn rightDownAdj

lddiagonal :: Cache -> Turn -> Word64 -> Board -> ([Word64], Cache)
lddiagonal cache turn = diagonal cache turn leftDownAdj

removeFromBoard :: Cache -> Board -> Turn -> Board
removeFromBoard Cache{..} Board{..} =
  \case
    Human    -> Board uppawns (downpawns `xor` pawn) upkings (downkings `xor` king) mask 
    Computer -> Board (uppawns `xor` pawn) downpawns (upkings `xor` king) downkings mask

stops :: Cache -> Turn -> Word64 -> Board -> Direction -> State
stops cache turn i board = \case
  NorthEast -> let (is, c) = rudiagonal cache turn i board
               in (is, c, NorthEast, removeFromBoard c board turn)
  NorthWest -> let (is, c) = ludiagonal cache turn i board
               in (is, c, NorthWest, removeFromBoard c board turn)
  SouthEast -> let (is, c) = rddiagonal cache turn i board
               in (is, c, SouthEast, removeFromBoard c board turn)
  SouthWest -> let (is, c) = lddiagonal cache turn i board
               in (is, c, SouthWest, removeFromBoard c board turn)

----------------------

type State = ([Word64], Cache, Direction, Board)

filterNullMap :: (Direction -> State) -> [Direction] -> [([Word64], Cache, Direction, Board)]
filterNullMap _ [] = []
filterNullMap f (x : xs)
  | null (first x') = filterNullMap f xs 
  | otherwise       = x' : filterNullMap f xs
  where x' = f x 
        first (y,_,_,_) = y

concatFilter ::  (a -> (Cache, [c])) -> [a] -> (Cache, [c])
concatFilter f = foldl' h (mempty,[])
  where h (c, mvs) i 
          | c' > c    = (c', mvs')
          | c' < c    = (c, mvs)
          | otherwise = (c, mvs ++ mvs')
          where (c', mvs') = f i 
-----------------------
-- Computer Simple Pawn Jump 

cpawnjump :: Turn -> Word64 -> Board -> (Cache, [AMove]) 
cpawnjump turn n board = start n mempty 
  where 
    start i m = 
      case (leftEat board i turn, rightEat board i turn) of 
        ((_,0),(_,0))     -> mempty 
        ((_,0),(c,t))     -> consA m $ go t c 
        ((c,t),(_,0))     -> consA m $ go t c 
        ((c1,t1),(c2,t2)) -> consA m $ go t1 c1 `append` go t2 c2 
    go !i m = 
      case (leftEat board i turn, rightEat board i turn) of 
        ((_,0),(_,0))     -> (m, [AMove Pawn (n .|. i) m])
        ((_,0),(c,t))     -> consA m $ go t c 
        ((c,t),(_,0))     -> consA m $ go t c 
        ((c1,t1),(c2,t2)) -> consA m $ go t1 c1 `append` go t2 c2 

-----------------------
-- Human Simple Pawn Jump 

hpawnjump :: Word64 -> Board -> (Cache, [HMove]) 
hpawnjump n board = start n 
  where 
    start i = 
      case (leftEat board i Human, rightEat board i Human) of 
        ((_,0),(_,0))     -> mempty 
        ((_,0),(c,t))     -> fmap (HMove Pawn i) <$> go t c 
        ((c,t),(_,0))     -> fmap (HMove Pawn i) <$> go t c 
        ((c1,t1),(c2,t2)) -> fmap (HMove Pawn i) <$> go t1 c1 `append` go t2 c2 
    go i m = 
      case (leftEat board i Human, rightEat board i Human) of 
        ((_,0),(_,0))     -> (m, [Point i m :| []])
        ((_,0),(c,t))     -> consH m i $ go t c 
        ((c,t),(_,0))     -> consH m i $ go t c 
        ((c1,t1),(c2,t2)) -> consH m i $ go t1 c1 `append` go t2 c2 

-----------------------
-- Computer King Jump Move 

ckingjump :: Turn -> Word64 -> Board -> (Cache, [AMove])
ckingjump turn i board = second nub begin 
  where
   begin = filterNullMap (stops mempty turn i board) [NorthEast ..]
         & concatFilter (\l@(_,c,_,_) -> apply c (`removestart` turn) l)
    
   go c dir board' j = 
     case stops c turn j board' `filterNullMap` alloweddirections dir of 
       [] -> (mempty, [AMove King (i .|. j) mempty])
       xs -> concatFilter (\l@(_,c',_,_) -> apply (c' <> c) id l) xs 
   
   apply c' f (is,c,dir,b) = consA c $ concatFilter (go (c <> c') dir (f b)) is 

   removestart Board{..} = \case 
     Human    -> Board uppawns downpawns (upkings `xor` i) downkings mask 
     Computer -> Board uppawns downpawns upkings (downkings `xor` i) mask

------------------------
-- Human King Jump Move 

hkingjump :: Word64 -> Board -> (Cache, [HMove])
hkingjump i board = second nub begin 
  where
   begin = filterNullMap (stops mempty Human i board) [NorthEast ..]
         & concatFilter (second (HMove King i <$>) . apply mempty removestart)
    
   go c dir board' j = 
     case stops chr Human j board' `filterNullMap` alloweddirections dir of 
       [] -> (fst c, [Point j (snd c) :| []])
       xs -> concatFilter (\l@(_,c',_,_) -> consH (snd c) j $ apply (c' <> chr) id l) xs 
     where chr = uncurry (<>) c
   
   apply c' f (is,c,dir,b) = concatFilter (go (c <> c', c) dir (f b)) is 

   removestart Board{..} = Board uppawns downpawns (upkings `xor` i) downkings mask 

-----------------------------
-- All Simple Pawn jumps 

allcpawnjumps :: Board -> Turn -> (Cache,[AMove])
allcpawnjumps board@Board{..} Computer = 
  if downpawns == 0 then mempty 
    else concatFilter (flip (cpawnjump Computer) board) $ availablepawnjumps' board Computer 
allcpawnjumps board@Board{..} Human = 
  if uppawns == 0 then mempty 
    else concatFilter (flip (cpawnjump Human) board) $ availablepawnjumps' board Human 

allhpawnjumps :: Board -> (Cache,[HMove]) 
allhpawnjumps board = concatFilter (`hpawnjump` board) $ availablepawnjumps' board Human 


availablepawnjumps' :: Board -> Turn -> [Word64]
availablepawnjumps' b@Board{..} = \case 
  Human    -> 
    let lefts  = (((es `shiftR` 5) .&. downs) `shiftR` 5) .&. uppawns 
        rights = (((es `shiftR` 4) .&. downs) `shiftR` 4) .&. uppawns 
    in decomp' $ lefts .|. rights 
  Computer -> 
    let lefts  = (((es `shiftL` 4) .&. ups) `shiftL` 4) .&. downpawns 
        rights = (((es `shiftL` 5) .&. ups) `shiftL` 5) .&. downpawns 
    in decomp' $ lefts .|. rights 
  where es    = empties b
        downs = downpawns .|. downkings
        ups   = uppawns   .|. upkings 

decomp :: (FiniteBits a) => a -> [a] 
decomp w64 = go (countTrailingZeros w64) (popCount w64)
  where 
    go _ 0 = []
    go i n | testBit w64 i = bit i : go (succ i) (pred n) 
           | otherwise     = go (succ i) n 

decomp' :: Word64 -> [Word64]
decomp' w64 = go 0
  where
    go e | w64 < power  = []
         | w64 .&. power /= 0  = power : go (e + 1)
         | otherwise    = go (e + 1)
         where power = shiftL 1 e

-----------------------------
-- All King Jump Moves 

-- # This can be improved TODO
allckingjumps :: Board -> Turn -> (Cache, [AMove])
allckingjumps b@Board{..} turn = 
  case availablekingjumps b turn of 
    [] -> mempty 
    _  -> go (countTrailingZeros kgs) (popCount kgs)
  where kgs = case turn of 
                Human    -> upkings 
                Computer -> downkings
        
        go _ 0 = mempty 
        go !i !n | testBit kgs i = ckingjump turn (bit i) b `append` go (succ i) (pred n)
                 | otherwise     = go (succ i) n 
  
allhkingjumps :: Board -> (Cache, [HMove])
allhkingjumps b@Board{..} = 
  case availablekingjumps b Human of 
    [] -> mempty 
    _  -> go (countTrailingZeros upkings) (popCount upkings)
  where 
    go _ 0 = mempty 
    go !i !n | testBit upkings i = hkingjump (bit i) b `append` go (succ i) (pred n)
             | otherwise         = go (succ i) n 

availablekingjumps :: Board -> Turn -> [Word64]
availablekingjumps board@Board{..} turn = 
  if (turn == Human && upkings == 0)  || (turn == Computer && downkings == 0) then []
        else            filter (checknextemptylu es) (checkjumpsleftUp    board turn)
                     <> filter (checknextemptyru es) (checkjumpsRightUp   board turn)
                     <> filter (checknextemptyld es) (checkjumpsLeftDown  board turn)
                     <> filter (checknextemptyrd es) (checkjumpsrightDown board turn)
  where es = empties board

checknextemptylu :: Word64 -> Word64 -> Bool 
checknextemptylu es ws = (ws `shiftL` 4) .&. es /= 0

checknextemptyru :: Word64 -> Word64 -> Bool 
checknextemptyru es ws = (ws `shiftL` 5) .&. es /= 0

checknextemptyld :: Word64 -> Word64 -> Bool 
checknextemptyld es ws = (ws `shiftR` 5) .&. es /= 0

checknextemptyrd :: Word64 -> Word64 -> Bool 
checknextemptyrd es ws = (ws `shiftR` 4) .&. es /= 0

checkjumpsleftUp :: Board -> Turn -> [Word64]
checkjumpsleftUp Board{..} = \case 
  Human    -> go (upkings `shiftL` 4)
    where go 0 = []
          go n = let j = n .&. opponents
                 in if j == 0 then go ((n `shiftL` 4) .&. ehs)
                              else j : go ((n `shiftL` 4) .&. ehs)
          opponents = downkings .|. downpawns          
  Computer -> go (downkings `shiftL` 4) 
    where go 0 = []
          go n = let j = n .&. opponents
                 in if j == 0 then go ((n `shiftL` 4) .&. ecs) 
                              else j : go ((n `shiftL` 4) .&. ecs)
          opponents = upkings .|. uppawns          
  where ehs = complement $ upkings   .|. uppawns   .|. mask
        ecs = complement $ downkings .|. downpawns .|. mask

checkjumpsrightDown :: Board -> Turn -> [Word64]
checkjumpsrightDown Board{..} = \case 
  Human    -> go (upkings `shiftR` 4)
    where go 0 = []
          go n = let j = n .&. opponents
                 in if j == 0 then go ((n `shiftR` 4) .&. ehs)
                              else j : go ((n `shiftR` 4) .&. ehs)
          opponents = downkings .|. downpawns          
  Computer -> go (downkings `shiftR` 4) 
    where go 0 = []
          go n = let j = n .&. opponents
                 in if j == 0 then go ((n `shiftR` 4) .&. ecs) 
                              else j : go ((n `shiftR` 4) .&. ecs)
          opponents = upkings .|. uppawns          
  where ehs = complement $ upkings   .|. uppawns   .|. mask
        ecs = complement $ downkings .|. downpawns .|. mask

checkjumpsRightUp :: Board -> Turn -> [Word64]
checkjumpsRightUp Board{..} = \case 
  Human    -> go (upkings `shiftL` 5)
    where go 0 = []
          go n = let j = n .&. opponents
                 in if j == 0 then go ((n `shiftL` 5) .&. ehs)
                              else j : go ((n `shiftL` 5) .&. ehs)
          opponents = downkings .|. downpawns          
  Computer -> go (downkings `shiftL` 5) 
    where go 0 = []
          go n = let j = n .&. opponents
                 in if j == 0 then go ((n `shiftL` 5) .&. ecs) 
                              else j : go ((n `shiftL` 5) .&. ecs)
          opponents = upkings .|. uppawns          
  where ehs = complement $ upkings   .|. uppawns   .|. mask
        ecs = complement $ downkings .|. downpawns .|. mask

checkjumpsLeftDown :: Board -> Turn -> [Word64]
checkjumpsLeftDown Board{..} = \case 
  Human    -> go (upkings `shiftR` 5)
    where go 0 = []
          go n = let j = n .&. opponents
                 in if j == 0 then go ((n `shiftR` 5) .&. ehs)
                              else j : go ((n `shiftR` 5) .&. ehs)
          opponents = downkings .|. downpawns          
  Computer -> go (downkings `shiftR` 5) 
    where go 0 = []
          go n = let j = n .&. opponents
                 in if j == 0 then go ((n `shiftR` 5) .&. ecs) 
                              else j : go ((n `shiftR` 5) .&. ecs)
          opponents = upkings .|. uppawns          
  where ehs = complement $ upkings   .|. uppawns   .|. mask
        ecs = complement $ downkings .|. downpawns .|. mask


----------------------
-- Make Jump Move 

mkJumpMove :: Board -> AMove -> Turn -> Board 
mkJumpMove Board{..} AMove{..} = \case 
  Human    -> 
    case size of 
      Pawn -> 
        let newuppawns   = uppawns `xor`path `xor` (path .&. upperRow)
            newdownpawns = downpawns `xor` pawn cache 
            newdownkings = downkings `xor` king cache 
            newupkings   = upkings .|. (path .&. upperRow)
        in Board newuppawns newdownpawns newupkings newdownkings mask
      King -> 
        let newupkings   = if popCount path == 1 then upkings else upkings `xor` path
            newdownpawns = downpawns `xor` pawn cache 
            newdownkings = downkings `xor` king cache 
        in Board uppawns newdownpawns newupkings newdownkings mask 
  Computer -> 
    case size of 
      Pawn ->
        let newdownpawns = downpawns `xor` path `xor` (path .&. lowerRow)
            newuppawns   = uppawns `xor` pawn cache
            newupkings   = upkings `xor` king cache 
            newdownkings = downkings .|. (path .&. lowerRow)
        in Board newuppawns newdownpawns newupkings newdownkings mask
      King -> 
        let newdownkings = if popCount path == 1 then downkings else downkings `xor` path 
            newuppawns   = uppawns `xor` pawn cache
            newupkings   = upkings `xor` king cache 
        in Board newuppawns downpawns newupkings newdownkings mask

rvJumpMove :: AMove -> Board -> Turn -> Board
rvJumpMove AMove{..} Board{..} = \case
  Computer ->
    case size of 
      Pawn ->
        let newuppawns = uppawns `xor` path `xor` (path .&. upperRow) 
            newdownpawns = downpawns .|. pawn cache
            newdownkings = downkings .|. king cache
            newupkings = upkings `xor` (path .&. upperRow)
        in Board newuppawns newdownpawns newupkings newdownkings mask 
      King -> 
        let newupkings = if popCount path == 1 then upkings else upkings `xor` path
            newdownpawns = downpawns .|. pawn cache
            newdownkings = downkings .|. king cache
        in Board uppawns newdownpawns newupkings newdownkings mask 
  Human    ->
    case size of 
      Pawn -> 
        let newdownpawns = downpawns `xor` path `xor` (path .&. lowerRow)
            newuppawns = uppawns .|. pawn cache
            newupkings = upkings .|. king cache
            newdownkings = downkings `xor`(path .&. lowerRow)
        in Board newuppawns newdownpawns newupkings newdownkings mask 
      King -> 
        let newdownkings = if popCount path == 1 then downkings else downkings `xor` path
            newuppawns = uppawns .|. pawn cache
            newupkings = upkings .|. king cache
        in Board newuppawns downpawns newupkings newdownkings mask

