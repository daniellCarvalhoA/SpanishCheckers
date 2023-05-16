{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module SimpleMove (
       simplePawnMove
     , simpleKingMove
     , mkSimpleMove
     , rvSimpleMove
     ) where

import Game             (Board(..), Turn (..), Size (..), nofRows, cellsPerRow)
import Utils            (rup, lup, rdp, ldp)
import Data.Word        (Word8, Word32)
import Data.List        (unfoldr)
import Data.Maybe       (catMaybes)
import Data.Bits        (Bits(..))
import Data.Tuple.Extra (both)

------------
--- Simple Pawn Move

simplePawnMove :: Word8 -> Turn -> Board  -> [(Word8, Word8)]
simplePawnMove n turn board = catMaybes $
    case turn of
        Human    -> [(n,) <$> lup board n, (n,) <$> rup board n]
        Computer -> [(n,) <$> ldp board n, (n,) <$> rdp board n]

-----------
---- Simple King Move 

diagonal :: (Board -> Word8 -> Maybe Word8) -> Word8 -> Board -> [(Word8, Word8)]
diagonal fun i = go i 
    where go m board = 
            unfoldr (\x -> case fun board x of 
                            Just t  -> Just ((i, t), t)
                            Nothing -> Nothing
            ) m

rudiagonal :: Word8 -> Board -> [(Word8, Word8)]
rudiagonal = diagonal rup 

ludiagonal :: Word8 -> Board -> [(Word8, Word8)]
ludiagonal = diagonal lup 

rddiagonal :: Word8 -> Board -> [(Word8, Word8)]
rddiagonal = diagonal rdp 

lddiagonal :: Word8 -> Board -> [(Word8, Word8)]
lddiagonal = diagonal ldp 

simpleKingMove :: Word8 -> Board -> [(Word8, Word8)]
simpleKingMove i board = rudiagonal i board <> (ludiagonal i board <> (rddiagonal i board <> lddiagonal i board))

-----------------------------
---- Make Simple Move 

swap :: Word32 -> (Int, Int) -> Word32
swap es (begin, end) = clearBit (setBit es begin) end

atLowerRow :: Word8 -> Bool 
atLowerRow x = x `div` cellsPerRow == 0

atUpperRow :: Word8 -> Bool 
atUpperRow x = x `div` cellsPerRow == nofRows

mkSimpleMove :: Turn -> Board -> Size -> (Word8, Word8) -> Board
mkSimpleMove Human Board{..} Pawn mv
    | atUpperRow (snd mv) = Board empties (clearBit ups start) downs (setBit kups end) kdowns 
    | otherwise           = Board empties (setBit (clearBit ups start) end) downs kups kdowns 
    where empties = swap emptys (start,end); (start, end) = both fromIntegral mv
mkSimpleMove Human Board{..} King mv
                          = Board empties ups downs (setBit (clearBit kups start) end) kdowns
    where empties = swap emptys (start,end); (start, end) = both fromIntegral mv
mkSimpleMove Computer Board{..} Pawn mv
    | atLowerRow (snd mv) = Board empties ups (clearBit downs start) kups (setBit kdowns end)
    | otherwise           = Board empties ups (setBit (clearBit downs start) end) kups kdowns
    where empties = swap emptys (start,end); (start, end) = both fromIntegral mv
mkSimpleMove Computer Board{..} King mv
                          = Board empties ups downs kups (setBit (clearBit kdowns start) end)
    where empties = swap emptys (start,end); (start, end) = both fromIntegral mv

------------------
----- Reverse SimpleMove 

rvSimpleMove :: Turn -> Board -> Size -> (Word8, Word8) -> Board 
rvSimpleMove Human    Board{..} Pawn mv
    | atUpperRow (snd mv) = Board empties (setBit ups start) downs (clearBit kups end) kdowns
    | otherwise           = Board empties (clearBit (setBit ups start) end) downs kups kdowns
    where empties = swap emptys (end, start) ; (start, end) = both fromIntegral mv
rvSimpleMove Human    Board{..} King mv 
                          = Board empties ups downs (clearBit (setBit kups start) end) kdowns
    where empties = swap emptys (end, start) ; (start, end) = both fromIntegral mv
rvSimpleMove Computer Board{..} Pawn mv 
    | atLowerRow (snd mv) = Board empties ups (setBit downs start) kups (clearBit kdowns end)
    | otherwise           = Board empties ups (clearBit (setBit downs start) end) kups kdowns
    where empties = swap emptys (end, start) ; (start, end) = both fromIntegral mv
rvSimpleMove Computer Board{..} King mv 
                          = Board empties ups downs kups (clearBit (setBit kdowns start) end)
    where empties = swap emptys (end, start) ; (start, end) = both fromIntegral mv