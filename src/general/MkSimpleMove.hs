{-# LANGUAGE RecordWildCards #-}

module MkSimpleMove where

import Data.Word (Word8)
import Game (Size (..), Board (..), Turn (..), atUpperRow, swap, atLowerRow)
import Data.Bits (Bits(..))
import Data.Tuple.Extra (both)


-----------------------------
---- Make Simple Move 


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