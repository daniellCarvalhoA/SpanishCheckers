{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RecordWildCards #-}

module Board (Board(..), testy, test2, testx, test4, test3,test5, testGalo,
              empties, testab, testab2 , testJump, testJump2, initialboard, test1, upperRow, lowerRow) where

import Data.Word       (Word64)
import Data.Bits       (Bits(..))
import Test.QuickCheck (Arbitrary(..), sublistOf, shuffle, chooseInt)
import Data.Foldable   (foldl')
import Data.List       ((\\), intercalate, zip4, intersperse)

data Board = Board {
    uppawns   :: !Word64 
  , downpawns :: !Word64
  , upkings   :: !Word64 
  , downkings :: !Word64
  , mask      :: !Word64
  } deriving Eq

empties :: Board -> Word64 
empties Board{..} = complement $ uppawns .|. downpawns .|. upkings .|. downkings .|. mask

instance Arbitrary Board where 
  arbitrary = do 
    (wp,wk) <- do 
      numberofups     <- chooseInt (0,11)
      partupawnskings <- chooseInt (0, numberofups)
      possibleups     <- sublistOf possible >>= shuffle 
      return $ splitAt partupawnskings $ take numberofups possibleups 
    
    (bp,bk) <- do 
      numberofdowns   <- chooseInt (0,11)
      partdpawnskings <- chooseInt (0, numberofdowns)
      possibledowns   <- sublistOf (possible \\ (wp <> wk)) >>= shuffle 
      return $ splitAt partdpawnskings $ take numberofdowns possibledowns 

    let wps = setBits wk .&. complement upperRow 
        wks = setBits wp 
        bps = setBits bp .&. complement lowerRow 
        bks = setBits bk 
    
    return $ Board wps bps wks bks msk   

   where possible = [0..7] <> [9..16] <> [18..25] <> [27..34]

setBits :: (Num a, Bits a) => [Int] -> a 
setBits = foldl' setBit 0 

msk :: Word64 
msk = setBits $ [8,17,26] <> [35 .. 63]

upperRow :: Word64 
upperRow = 0x780000000 

lowerRow :: Word64
lowerRow = 0xf  
----------------------------------------
---- Board Pretty printing 

instance Show Board where
  show Board{..} = 
    let ws  = pad35 $ binary $ uppawns 
        bs  = pad35 $ binary $ downpawns 
        kws = pad35 $ binary $ upkings 
        kbs = pad35 $ binary $ downkings 
        vec = removeAll [26,17,8] $ zip4 ws bs kws kbs 
        brd = zip [0..] $ map reverse $ groupAt 4 vec 
        g (1,1,1,1) = "|#|"
        g (1,0,0,0) = "|u|"
        g (0,1,0,0) = "|d|"
        g (0,0,1,0) = "|U|"
        g (0,0,0,1) = "|D|"
        g (0,0,0,0) = "|_|" 
        g s         = error $ "Failure: " <> show s 
        f :: (Int,[(Word64,Word64,Word64,Word64)]) -> [(Word64,Word64,Word64,Word64)]
        f (i,xs) | odd i     = (1,1,1,1) : intersperse (1,1,1,1) xs 
                 | otherwise = (<> [(1,1,1,1)]) $ intersperse (1,1,1,1) xs 
    in intercalate "\n" $ concatMap g . f <$> brd 

binary :: Integral t => t -> [t]
binary m = go m id []
  where 
    go 0 k = k 
    go n k = go q ((r:) . k)
      where (q,r) = n `divMod` 2 

groupAt :: Int -> [a] -> [[a]]
groupAt _ [] = [] 
groupAt n xs = ys : groupAt n zs 
   where (ys,zs) = splitAt n xs 

pad35 :: Integral t => [t] -> [t] 
pad35 ts = 
  let l = abs (35 - length ts) 
  in replicate l 0 <> ts 

remove :: Int -> [a] -> [a] 
remove _ [] = []
remove 0 (_ : xs) = xs 
remove n (x : xs) = x : remove (n - 1) xs 

removeAll :: [Int] -> [a] -> [a]
removeAll is xs = foldl' (flip remove) xs is 

----------------------
-- Board 

initialboard :: Board 
initialboard = Board (setBits $ [0..7] <> [9..12]) (setBits $ [22..25] <> [27..34]) 0 0 msk                  

testx :: Board 
testx = Board  (setBits [13]) (setBits [33,32,28,27,23,22,18]) (bit 34) 0 msk 

testy :: Board 
testy = Board (setBits [0,2,4,6,11,12,13,22]) (setBits [18,21,24,25,30,31,33,34]) 0 0 msk


testJump2 :: Board 
testJump2 = Board (setBits $ [0..7] <> [9..12]) (setBits $ [14] <> [23..25] <> [27..34]) 0 0 msk                  

test1 :: Board 
test1 = Board 0 (setBits [19,29]) (setBits [1,11,12,13,17,34,32]) (setBits [6,7,14,31,21]) msk

test2 :: Board 
test2 = Board 0 (setBits [29,24]) 0 (bit 25) msk

test3 :: Board 
test3 = Board (setBits [5,6,7,9,10,12,14]) (setBits [16,19,20,21,22,24,25,29]) (bit 13 ) 0 msk 

test4  :: Board 
test4 = Board (setBits [4,5,7,9,10,12,14]) (setBits [16,19,20,21,22,24,25,29]) (bit 23) 0 msk

test5 :: Board 
test5 = Board (setBits [0,2,4,6,7,9,10,12,15,19]) (setBits [14,18,21,24,25,27,28,29,33,34]) 0 0 msk 

testJump :: Board 
testJump = Board (bit 1) (bit 5) 0 0 msk

testGalo :: Board 
testGalo = Board 0 0 (setBits [24]) (setBits [3,22,25]) msk

testab :: Board 
testab = Board (setBits [9,14,16] ) (setBits [13,18,19,21,22,24,25,27,29]) 0 0 msk 

testab2 :: Board 
testab2 = Board (setBits [0,1,2,4,10,14,24]) 
                (setBits [34,31,27,18]) 0 (bit 23) msk 


---------------
-- ----- Board Type

-- data Board = Board {
-- emptys :: Word32
-- , ups    :: Word32
-- , downs  :: Word32
-- , kups   :: Word32
-- , kdowns :: Word32
-- } deriving (Show, Eq)

-- data Board2 = Board2 {
-- bws :: Word32
-- , bbs :: Word32
-- , bks :: Word32
-- }

-- instance Show Board2 where
-- show Board2{..} =
-- let ws  = pad32 $ binary $ bws `xor` bks
-- bs  = pad32 $ binary $ bbs `xor` bks
-- kws = pad32 $ binary $ bws .&.   bks
-- kbs = pad32 $ binary $ bbs .&.   bks
-- vec = zip4 ws bs kws kbs
-- brd = zip [0 ..] $ groupAt 4 vec
-- f (i,xs)
-- | odd i    = (1,1,1,1) : intersperse (1,1,1,1) xs
-- | otherwise = (<> [(1,1,1,1)]) $ intersperse (1,1,1,1) xs

-- g (1,1,1,1) = "|#|"
-- g (1,0,0,0) = "|w|"
-- g (0,1,0,0) = "|b|"
-- g (0,0,1,0) = "|W|"
-- g (0,0,0,1) = "|B|"
-- g (0,0,0,0) = "|_|"
-- g _         = error "never supposed to happen"
-- in intercalate "\n" $ concatMap g . f <$> brd

-- empties :: Board2 -> Word32
-- empties Board2{..}    = complement $ bws .|. bks

-- blackings :: Board2 -> Word32
-- blackings Board2{..}  = bbs .&. bks

-- whitekings :: Board2 -> Word32
-- whitekings Board2{..} = bws .&. bks

-- initialboard2 :: Board2
-- initialboard2 = Board2 (setBits [0,1,2,3,4,5,6,7,8,9,10,11]) (setBits [20,21,22,23,24,25,26,27,28,29,30,31]) 0

-- ---- Pretty Print Board

-- showBoard :: (Turn, Color) -> Board -> String
-- showBoard asc Board{..} =
-- foldr (zipWith m . uncurry showPiece) (repeat '_')
-- [(' ', emptys),(f asc Pawn Human, ups),(f asc Pawn Computer, downs),(f asc King Human, kups), (f asc King Computer, kdowns)]
-- where
-- m '_' x = x
-- m x '_' = x
-- m x _   = x

-- f (Human, White)    Pawn Human    = 'w'
-- f (Computer, Black) Pawn Human    = 'w'
-- f (Human, White)    King Human    = 'W'
-- f (Computer, Black) King Human    = 'W'
-- f (Human, White)    Pawn Computer = 'b'
-- f (Computer, Black) Pawn Computer = 'b'
-- f (Human, White)    King Computer = 'B'
-- f (Computer, Black) King Computer = 'B'
-- f (Human, Black)    Pawn Human    = 'b'
-- f (Computer, White) Pawn Human    = 'b'
-- f (Human, Black)    King Human    = 'B'
-- f (Computer, White) King Human    = 'B'
-- f (Human, Black)    Pawn Computer = 'w'
-- f (Computer, White) Pawn Computer = 'w'
-- f (Human, Black)    King Computer = 'W'
-- f (Computer,White)  King Computer = 'W'

-- showPiece :: Char -> Word32 -> String
-- showPiece c w32 = unlines $ zipWith toRow [0 ..]
-- ( map reverse $ groupAt 4
-- $ map (\x -> if x == 1 then c else '_')
-- $ pad32 $ binary w32
-- )

-- toRow :: Int ->  String -> String
-- toRow n str | even n    = "|" <> intercalate "|#|" (map (:[]) str) <> "|#|" <> show (31 - 4 * n)
-- | otherwise = "|#|" <> intercalate "|#|" (map (:[]) str) <> "|" <> show (31 - 4 * n)

-- pad32 :: Num a => [a] -> [a]
-- pad32 xs = replicate (32 - n) 0 <> xs
-- where n = length xs

-- binary :: Integral t => t -> [t]
-- binary m = go m id []
-- where
-- go 0 k = k
-- go n k = go q ((r:) . k)
-- where (q,r) = n `divMod` 2

-- groupAt :: Int -> [a] -> [[a]]
-- groupAt _ [] = []
-- groupAt n xs = zs : groupAt n ys
-- where (zs, ys) = splitAt n xs


---





-- cellsPerRow :: Word8
-- cellsPerRow = 4

-- nofCells :: Word8
-- nofCells = 31 -- counting from 0

-- nofRows :: Word8
-- nofRows = 7  -- counting from 0

-- swap :: Word32 -> (Int, Int) -> Word32
-- swap es (begin, end) = clearBit (setBit es begin) end

-- atLowerRow :: Word8 -> Bool
-- atLowerRow x = x `div` cellsPerRow == 0

-- atUpperRow :: Word8 -> Bool
-- atUpperRow x = x `div` cellsPerRow == nofRows

-- ----------- Arbitrary Board

-- instance Arbitrary Board where
-- arbitrary :: Gen Board
-- arbitrary = margins <$> gboard

-- margins :: Board -> Board
-- margins Board{..} =
-- let ups'    = ups    .&. (maxBound `shiftR` 4)
-- kups'   = kups   .|. ((ups `shiftR` 0x1C) `shiftL` 28)
-- downs'  = downs  .&. (maxBound `shiftL` 4)
-- kdowns' = kdowns .|. ((downs `shiftL` 28) `shiftR` 28)
-- in Board emptys ups' downs' kups' kdowns'

-- gboard :: Gen Board
-- gboard = do
-- (l1, es, pick1) <- fromRest [] 31 31 8
-- (l2, ws, pick2) <- fromRest pick1 (31 - l1) 11 0
-- (l3, bs, pick3) <- fromRest pick2 (31 - l1 - l2) 11 0
-- (_, kws, pick4) <- fromRest pick3 (31 - l1 - l2 - l3) 11 0
-- let kbs = [0 .. 31] \\ pick4
-- return $ Board (setBits es) (setBits ws) (setBits bs) (setBits kws) (setBits kbs)

-- setBits :: (Foldable t, Bits b, Num b) => t Int -> b
-- setBits = foldl' setBit 0

-- fromRest :: [Int] -> Int -> Int -> Int -> Gen (Int, [Int], [Int])
-- fromRest picked left mx mn = do
-- i  <- chooseInt (mn, left `min` mx)
-- is <- upToMax i picked
-- return (i, is, picked <> is)

-- upToMax :: Int -> [Int] -> Gen [Int]
-- upToMax 0 _ = return []
-- upToMax n ps = do
-- l <- upToMax (n - 1) ps
-- i <- suchThat (chooseInt (0, 31)) (`notElem` (l <> ps))
-- return $ i : l

--------------------------------
-------------------------------

-- initialboard :: Board
-- initialboard = Board empties' ups' downs' kups' kdowns'
-- where
-- ups'     = 0xfff
-- downs'   = ups' `shiftL` 20
-- kups'    = 0
-- kdowns'  = 0
-- empties' = complement $ ups' .|. downs'

-- testBoard :: Board
-- testBoard = Board empties' ups' downs' kups' kdowns'
-- where
-- ups'     = bit 16
-- downs'   = bit 21
-- kups'    = bit 28
-- kdowns'  = 0
-- empties' = complement $ ups' .|. downs' .|. kups' .|. kdowns'

-- testBoard2 :: Board
-- testBoard2 = Board empties' ups' downs' kups' kdowns'
-- where
-- ups'     = setBit (setBit (bit 2) 3) 10
-- downs'   = bit 7
-- kups'    = 0
-- kdowns'  = 0
-- empties' = complement $ ups' .|. downs'

-- testBoard3 :: Board
-- testBoard3 = Board empties' ups' downs' kups' kdowns'
-- where
-- ups'     = 0
-- downs'   = 0 `setBit` 10 `setBit` 11 `setBit` 16 `setBit` 17 `setBit` 22 `setBit` 24
-- kups'    = bit 18
-- kdowns'  = 0
-- empties' = complement $ kups' .|. downs'

-- testBoard4 :: Board
-- testBoard4 = Board empties' ups' downs' kups' kdowns'
-- where
-- ups'     = setBits [0,2,4,6,7,8,9,11,14,17]
-- downs'   = setBits [13,16,19,22,23,24,25,26,30,31]
-- kups'    = 0
-- kdowns'  = 0
-- empties' = complement $ ups' .|. downs'

-- testBoard5 :: Board
-- testBoard5 = Board empties' ups' downs' kups' kdowns'
-- where
-- ups'     = 0
-- downs'   = 0
-- kups'    = setBits [22]
-- kdowns'  = setBits [3,20,23]
-- empties' = complement $ kups' .|. kdowns'

-- testBoard6 :: Board
-- testBoard6 = Board empties' ups' downs' kups' kdowns'
-- where
-- ups'     = setBits [4,7,8,9,10,11,12,15,19]
-- downs'   = setBits [17,18,20,21,22,25,26,27,30]
-- kups'    = 0
-- kdowns'  = 0
-- empties' = complement $ ups' .|. downs'

-- testBoard7 :: Board
-- testBoard7 = Board empties' ups' downs' kups' kdowns'
-- where
-- ups'     = 0
-- downs'   = 0
-- kups'    = setBits [2,3]
-- kdowns'  = 0
-- empties' = complement kups'

-- testBoard8 :: Board
-- testBoard8 = Board empties' ups' downs' kups' kdowns'
-- where
-- ups'     = setBits [1]
-- downs'   = setBits [5,13,14,21]
-- kups'    = 0
-- kdowns'  = setBits [6]
-- empties' = complement $ ups' .|. downs' .|. kdowns'
