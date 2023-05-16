{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Board (Board(..), nofCells, nofRows, cellsPerRow, swap, atLowerRow, atUpperRow, initialboard, showBoard, groupAt, testBoard )where

import Data.Word          (Word32, Word8)
import Data.Bits          ( Bits(..) )
import Test.QuickCheck (Arbitrary, Gen, chooseInt, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Data.Foldable (foldl')
import Data.List ((\\), intercalate)
import Types




-----------------
----- Board Type 

data Board = Board {
    emptys :: Word32 
  , ups    :: Word32
  , downs  :: Word32
  , kups   :: Word32
  , kdowns :: Word32
} deriving (Show, Eq)

---- Pretty Print Board 

showBoard :: Assoc -> Board -> String 
showBoard asc Board{..} = 
  foldr (zipWith m . uncurry showPiece) (repeat '_')
    [(' ', emptys),(f asc Pawn Human, ups),(f asc Pawn Computer, downs),(f asc King Human, kups), (f asc King Computer, kdowns)]
    where 
      m '_' x = x 
      m x '_' = x 
      m x _   = x 

      f (Human, White)    Pawn Human    = 'w'
      f (Computer, Black) Pawn Human    = 'w'
      f (Human, White)    King Human    = 'W'
      f (Computer, Black) King Human    = 'W'
      f (Human, White)    Pawn Computer = 'b'
      f (Computer, Black) Pawn Computer = 'b'
      f (Human, White)    King Computer = 'B'
      f (Computer, Black) King Computer = 'B'
      f (Human, Black)    Pawn Human    = 'b'
      f (Computer, White) Pawn Human    = 'b'
      f (Human, Black)    King Human    = 'B'
      f (Computer, White) King Human    = 'B'
      f (Human, Black)    Pawn Computer = 'w'
      f (Computer, White) Pawn Computer = 'w'
      f (Human, Black)    King Computer = 'W'
      f (Computer,White)  King Computer = 'W'
      
showPiece :: Char -> Word32 -> String 
showPiece c w32 = unlines $ zipWith toRow [0 ..]
                          ( map reverse $ groupAt 4
                          $ map (\x -> if x == 1 then c else '_')
                          $ pad32 $ binary w32
                          )

toRow :: Int ->  String -> String 
toRow n str | even n    = "|" <> intercalate "|#|" (map (:[]) str) <> "|#|" <> show (31 - 4 * n)
            | otherwise = "|#|" <> intercalate "|#|" (map (:[]) str) <> "|" <> show (31 - 4 * n) 

pad32 :: Num a => [a] -> [a]
pad32 xs = replicate (32 - n) 0 <> xs 
  where n = length xs 

binary :: Integral t => t -> [t]
binary m = go m id []
  where 
    go 0 k = k 
    go n k = go q ((r:) . k)
      where (q,r) = n `divMod` 2

groupAt :: Int -> [a] -> [[a]]
groupAt _ [] = []
groupAt n xs = zs : groupAt n ys
  where (zs, ys) = splitAt n xs


-----





cellsPerRow :: Word8
cellsPerRow = 4

nofCells :: Word8
nofCells = 31 -- counting from 0

nofRows :: Word8
nofRows = 7  -- counting from 0

swap :: Word32 -> (Int, Int) -> Word32
swap es (begin, end) = clearBit (setBit es begin) end

atLowerRow :: Word8 -> Bool 
atLowerRow x = x `div` cellsPerRow == 0

atUpperRow :: Word8 -> Bool 
atUpperRow x = x `div` cellsPerRow == nofRows

----------- Arbitrary Board 

instance Arbitrary Board where 
  arbitrary :: Gen Board
  arbitrary = margins <$> gboard

margins :: Board -> Board 
margins Board{..} = 
  let ups'    = ups    .&. (maxBound `shiftR` 4)
      kups'   = kups   .|. ((ups `shiftR` 0x1C) `shiftL` 28)
      downs'  = downs  .&. (maxBound `shiftL` 4)
      kdowns' = kdowns .|. ((downs `shiftL` 28) `shiftR` 28)
   in Board emptys ups' downs' kups' kdowns' 

gboard :: Gen Board 
gboard = do 
  (l1, es, pick1) <- fromRest [] 31 31 8
  (l2, ws, pick2) <- fromRest pick1 (31 - l1) 11 0
  (l3, bs, pick3) <- fromRest pick2 (31 - l1 - l2) 11 0 
  (_, kws, pick4) <- fromRest pick3 (31 - l1 - l2 - l3) 11 0 
  let kbs = [0 .. 31] \\ pick4
  return $ Board (setBits es) (setBits ws) (setBits bs) (setBits kws) (setBits kbs)

setBits :: (Foldable t, Bits b, Num b) => t Int -> b 
setBits = foldl' setBit 0 

fromRest :: [Int] -> Int -> Int -> Int -> Gen (Int, [Int], [Int])
fromRest picked left mx mn = do 
  i  <- chooseInt (mn, left `min` mx) 
  is <- upToMax i picked
  return (i, is, picked <> is)

upToMax :: Int -> [Int] -> Gen [Int]
upToMax 0 _ = return []
upToMax n ps = do 
  l <- upToMax (n - 1) ps 
  i <- suchThat (chooseInt (0, 31)) (`notElem` (l <> ps))
  return $ i : l

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

testBoard :: Board 
testBoard = Board tEmptys upJump downJump bupJump 0 

tEmptys = complement $ upJump .|. downJump .|. bupJump

upJump = bit 16

bupJump = bit 28 

downJump = bit 21
