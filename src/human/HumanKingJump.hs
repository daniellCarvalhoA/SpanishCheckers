{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}

module HumanKingJump where 

import Data.Bits          (testBit, setBit, clearBit, (.|.), popCount)
import Types
    ( Turn(..),
      HumanMove(Cons),
      Memoize(Memoize),
      Point(Point),
      Eaten(..),
      Direction(..),
      Cache(..),
      allowedDirections,
      cons',
      addToCache )
import Board ( Board(..) )
import Data.Word          (Word8, Word32) 
import Data.List          (unfoldr)
import Utils              ( leftDownAdj, leftUpAdj, rightDownAdj
                          , rightUpAdj, filterNullMap, concatFilter
                          )
import Data.List.NonEmpty (NonEmpty(..))
import Control.Arrow      (first, second)
import Data.Function      ((&))

untilObstacle :: Turn -> (Word8 -> Maybe Word8) 
              -> Word8 -> Board -> Maybe (Word8, Eaten)
untilObstacle turn f n b@Board{..} = 
  case f n of 
    Nothing -> Nothing 
    Just i 
      | testBit emptys j -> untilObstacle turn f i b
      | testBit (if turn == Human then downs  else  ups) j -> Just (i, Left  i)
      | testBit (if turn == Human then kdowns else kups) j -> Just (i, Right i)
      | otherwise -> Nothing
      where j = fromIntegral i

diagonal :: Word32 -> Turn -> (Word8 -> Maybe Word8) -> Word8 
         -> Board -> ([Word8], Eaten)
diagonal w32 turn f n b@Board{..} = 
  case untilObstacle turn f n b of 
    Nothing     -> ([],Left  0)
    Just (i, e) ->
      (unfoldr (\x -> case f x of 
        Nothing                   -> Nothing
        Just j | testBit w32    k -> Nothing
               | testBit emptys k -> Just (j,j)
               | otherwise        -> Nothing
               where k = fromIntegral j 
                 ) i, e)

rudiagonal :: Word32 -> Turn -> Word8 -> Board -> ([Word8], Eaten)
rudiagonal w32 turn = diagonal w32 turn rightUpAdj

ludiagonal :: Word32 -> Turn -> Word8 -> Board -> ([Word8], Eaten)
ludiagonal w32 turn = diagonal w32 turn leftUpAdj

rddiagonal :: Word32 -> Turn -> Word8 -> Board -> ([Word8], Eaten)
rddiagonal w32 turn = diagonal w32 turn rightDownAdj

lddiagonal :: Word32 -> Turn -> Word8 -> Board -> ([Word8], Eaten)
lddiagonal w32 turn = diagonal w32 turn leftDownAdj

removeFromBoard :: Eaten -> Board -> Turn -> Board
removeFromBoard (Left  w8) Board{..} = \case 
  Human    -> Board (setBit emptys i) ups (clearBit downs i) kups kdowns
  Computer -> Board (setBit emptys i) (clearBit ups i) downs kups kdowns
  where i = fromIntegral w8
removeFromBoard (Right w8) Board {..} = \case 
  Human    -> Board (setBit emptys i) ups downs kups (clearBit kdowns i)
  Computer -> Board (setBit emptys i) ups downs (clearBit kups i) kdowns
  where i = fromIntegral w8

type State = ([Word8], Eaten, Direction, Board)


 -- represents the state of a move at a specific instance
 -- [Word8]   := possible landing cells
 -- Eaten     := the opponents piece just eaten 
 -- Direction := The direction in which the eating piece came
 -- Board     := the state of the board after the (semi) move

stops :: Cache -> Turn -> Word8 -> Board -> Direction -> State 
stops Cache{..} turn w8 board = \case 
  NorthEast -> let (is, e) = rudiagonal (pawn .|. king) turn w8 board 
                in (is, e, NorthEast, removeFromBoard e board turn)
  NorthWest -> let (is, e) = ludiagonal (pawn .|. king) turn w8 board
                in (is, e, NorthWest, removeFromBoard e board turn)
  SouthEast -> let (is, e) = rddiagonal (pawn .|. king) turn w8 board 
                in (is, e, SouthEast, removeFromBoard e board turn)
  SouthWest -> let (is, e) = lddiagonal (pawn .|. king) turn w8 board 
                in (is, e, SouthWest, removeFromBoard e board turn) 

kingJumpMove :: Turn -> Board -> Word8 -> (Memoize, [HumanMove])
kingJumpMove turn board i = 
  first (\Cache{..} -> 
            let ps = fromIntegral $ popCount pawn 
                ks = fromIntegral $ popCount king 
             in Memoize ps ks
        ) begin 
  where 
    begin = filterNullMap (stops mempty turn i board) [NorthEast ..]
          & concatFilter (second (Cons i <$>) . apply mempty (`removeStart` turn))

    go c dir board' j = 
      case stops chr turn j board' `filterNullMap` allowedDirections dir of 
        [] -> (fst c, [Point j (snd c) :| []])
        xs -> concatFilter (\l@(_, c', _, _) -> 
                cons' (snd c) j $ apply (addToCache chr c') id l) xs 
      where chr = uncurry addToCache c 

    apply c' f (is, c'', d, b) = concatFilter (go (addToCache c' c'', c'') d (f b)) is

    removeStart Board{..} = \case 
      Human    -> Board (setBit emptys j) ups downs (clearBit kups j) kdowns 
      Computer -> Board (setBit emptys j) ups downs kups (clearBit kdowns j) 
    
    j = fromIntegral i