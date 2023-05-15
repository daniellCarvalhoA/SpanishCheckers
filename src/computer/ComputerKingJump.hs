{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}

module ComputerKingJump (kingJumpMove)  where 

import Data.Bits (testBit, (.|.), xor, Bits (bit, setBit, clearBit))
import Game      ( Cache(..), Turn(..), Board(..)
                 , cacheKing, cachePawn, Direction (..), ComputerMove (..), allowedDirections)
import Data.Word (Word8)
import Data.List (unfoldr, nub)
import Utils     ( rightUpAdj, leftUpAdj, rightDownAdj, leftDownAdj, filterNullMap, concatFilter ) 
import Control.Arrow (second)
import Data.Function ((&))


kingJumpMove :: Turn -> Board -> Word8 -> (Cache, [ComputerMove])
kingJumpMove turn board i = second nub begin 
  where
    begin = filterNullMap (stops mempty turn i board) [NorthEast ..]
          & concatFilter (\l@(_, c, _, _) -> apply c i (`removeStart` turn) l)
    
    go c dir board' j = case stops c turn j board' `filterNullMap` allowedDirections dir of 
                          [] -> (mempty, [ComputerMove j mempty (bit k) j])
                          xs -> concatFilter (\l@(_, c', _, _) -> apply (c' <> c) j id l) xs 
              where k = fromIntegral j 
    
    apply c' j f (is, c, dir, b) = consM c j $ concatFilter (go (c <> c') dir (f b)) is 

    removeStart Board{..} = \case 
      Human    -> Board (setBit emptys j) ups downs (clearBit kups j) kdowns 
      Computer -> Board (setBit emptys j) ups downs kups (clearBit kdowns j)
      where j = fromIntegral i

consM :: Cache -> Word8 -> (Cache, [ComputerMove]) -> (Cache, [ComputerMove])
consM c n (c', mvs) = (c <> c', consM' c n <$> mvs)

consM' :: Cache -> Word8 -> ComputerMove -> ComputerMove
consM' c n ComputerMove{..} = ComputerMove n (c <> cache) (setBit path m) end 
  where m = fromIntegral n 

untilObstacle :: Turn -> (Word8 -> Maybe Word8) -> Word8 -> Board -> Maybe (Word8, Cache)
untilObstacle turn f n b@Board{..} = 
  case f n of 
    Nothing -> Nothing 
    Just i 
      | testBit emptys j -> untilObstacle turn f i b 
      | testBit (if turn == Human then downs  else  ups) j -> Just (i, cachePawn i)
      | testBit (if turn == Human then kdowns else kups) j -> Just (i, cacheKing i)
      | otherwise -> Nothing
      where j = fromIntegral i

diagonal :: Cache -> Turn -> (Word8 -> Maybe Word8) -> Word8 -> Board -> ([Word8], Cache)
diagonal Cache{..} turn f n b@Board{..} = 
  case untilObstacle turn f n b of 
    Nothing     -> ([], mempty)
    Just (i, c) -> (,c) $ 
      unfoldr (\x -> case f x of 
        Nothing -> Nothing
        Just j | testBit (pawn .|. king) k -> Nothing
               | testBit emptys k          -> Just (j,j)
               | otherwise                 -> Nothing
               where k = fromIntegral j
               ) i

rudiagonal :: Cache -> Turn -> Word8 -> Board -> ([Word8], Cache)
rudiagonal cache turn = diagonal cache turn rightUpAdj

ludiagonal :: Cache -> Turn -> Word8 -> Board -> ([Word8], Cache)
ludiagonal cache turn = diagonal cache turn leftUpAdj

rddiagonal :: Cache -> Turn -> Word8 -> Board -> ([Word8], Cache)
rddiagonal cache turn = diagonal cache turn rightDownAdj

lddiagonal :: Cache -> Turn -> Word8 -> Board -> ([Word8], Cache)
lddiagonal cache turn = diagonal cache turn leftDownAdj

removeFromBoard :: Cache -> Board -> Turn -> Board 
removeFromBoard Cache{..} Board{..} = 
  \case 
    Human    -> Board empties ups (downs `xor` pawn) kups (kdowns `xor` king)
    Computer -> Board empties (ups `xor` pawn) downs (kups `xor` king) kdowns 
    where empties = emptys .|. pawn .|. king 

type State = ([Word8], Cache, Direction, Board)

stops :: Cache -> Turn -> Word8 -> Board -> Direction -> State 
stops cache turn i board = \case 
  NorthEast -> let (is, c) = rudiagonal cache turn i board 
                in (is, c, NorthEast, removeFromBoard c board turn)
  NorthWest -> let (is, c) = ludiagonal cache turn i board 
                in (is, c, NorthWest, removeFromBoard c board turn)
  SouthEast -> let (is, c) = rddiagonal cache turn i board  
                in (is, c, SouthEast, removeFromBoard c board turn)
  SouthWest -> let (is, c) = lddiagonal cache turn i board
                in (is, c, SouthWest, removeFromBoard c board turn)
