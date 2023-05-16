{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module MkJumpMove where 
import Game
import Data.Tuple.Extra (both)
import Data.Bits (Bits(..))

mkJumpMove :: Board -> Size -> ComputerMove -> Turn -> Board 
mkJumpMove Board{..} size ComputerMove{..} = \case 
  Human    -> 
    let (begin, finish) = both fromIntegral (start, end)
        downs'          = downs  `xor` pawn cache 
        kdowns'         = kdowns `xor` king cache
        es              = emptys .|. pawn cache .|. king cache 
        empties | start == end = es 
                | otherwise    = swap es (begin, finish)
        (ups', kups') 
          | start == end                   = (ups, kups)
          | size == Pawn && atUpperRow end = (clearBit ups begin, setBit kups finish)
          | size == Pawn                   = (swap ups (begin, finish), kups)
          | otherwise                      = (ups, swap kups (begin, finish))
     in Board empties ups' downs' kups' kdowns' 
  Computer -> 
    let (begin, finish) = both fromIntegral (start, end)
        ups'            = ups  `xor` pawn cache
        kups'           = kups `xor` king cache
        es              = emptys .|. pawn cache .|. king cache 
        empties | start == end = es 
                | otherwise    = swap es (begin, finish)
        (downs', kdowns') 
          | start == end                   = (downs, kdowns)
          | size == Pawn && atLowerRow end = (clearBit downs begin, setBit kdowns finish)
          | size == Pawn                   = (swap downs (begin, finish), kdowns)
          | otherwise                      = (downs, swap kdowns (begin, finish))
     in Board empties ups' downs' kups' kdowns'

----------------------
---- Reverse Jump Move 

rvJumpMove :: Board -> Size -> ComputerMove -> Turn -> Board 
rvJumpMove Board{..} size ComputerMove{..} = \case 
  Human ->
    let (begin, finish) = both fromIntegral (start, end)
        downs'          = downs  .|. pawn cache 
        kdowns'         = kdowns .|. king cache
        es              = emptys `xor` pawn cache `xor` king cache 
        empties | start == end = es 
                | otherwise    = if testBit (pawn cache .|. king cache) finish
                                    then clearBit (clearBit es begin) finish 
                                    else swap es (finish, begin) 
        (ups', kups')
          | start == end                   = (ups, kups)
          | size == Pawn && atUpperRow end = (setBit ups begin, clearBit kups finish)
          | size == Pawn                   = (swap ups (finish, begin), kups)
          | otherwise                      = (ups, swap kups (finish, begin))
     in Board empties ups' downs' kups' kdowns' 
  Computer -> 
    let (begin, finish) = both fromIntegral (start, end)
        ups'            = downs  .|. pawn cache 
        kups'           = kdowns .|. king cache
        es              = emptys `xor` pawn cache `xor` king cache 
        empties | start == end = es 
                | otherwise    = if testBit (pawn cache .|. king cache) finish
                                   then clearBit (clearBit es begin) finish 
                                   else swap es (finish, begin)
        (downs', kdowns') 
          | start == end                   = (downs, kdowns)
          | size == Pawn && atLowerRow end = (setBit downs begin, clearBit kdowns finish)
          | size == Pawn                   = (swap downs (finish, begin), kdowns)
          | otherwise                      = (downs, swap kdowns (finish, finish))
     in Board empties ups' downs' kups' kdowns'   