{-# LANGUAGE TupleSections   #-}

module SimpleMove (
       simplePawnMove
     , simpleKingMove
     ) where

import Game             (Board(..), Turn (..))
import Utils            (rup, lup, rdp, ldp)
import Data.Word        (Word8)
import Data.List        (unfoldr)
import Data.Maybe       (catMaybes)

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

