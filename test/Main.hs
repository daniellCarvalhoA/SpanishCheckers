{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Main where 

import Test.QuickCheck.Arbitrary () 
import Test.QuickCheck.All       (quickCheckAll)
import Board                     (Board(..))
import Data.Bits                 (Bits(..))
import Data.Functor              (void,(<&>))
import Data.Function             ((&))
import Data.List.NonEmpty        (NonEmpty(..))
import Game                      (Game(..))
import AlphaBeta                 (extendGame, devolveGame)
import Types                     (Turn(..))

prop_disjoint :: Board -> Bool 
prop_disjoint Board{..} = 
     all ((== 0) . (emptys .&.)) [ups,downs,kups,kdowns]
  && all ((== 0) . (ups    .&.)) [downs,kups,kdowns]
  && all ((== 0) . (downs  .&.)) [kups,kdowns]
  && kups .&. kdowns == 0

prop_complete :: Board -> Bool 
prop_complete Board{..} = emptys .|. ups .|. downs .|. kups .|. kdowns == maxBound

prop_iso :: Game -> Bool
prop_iso g@Game{..} | fst assoc == Human = True 
                    | otherwise = extendGame g <&> devolveGame & allEqual

allEqual :: Eq a => NonEmpty a -> Bool 
allEqual  (h :| hs) = all (== h) hs

prop_all_disjoint :: Game -> Bool 
prop_all_disjoint g = all (prop_disjoint . board) $ extendGame g 

prop_all_complete :: Game -> Bool 
prop_all_complete g = all (prop_complete . board) $ extendGame g 

prop_all_complete2 :: Game -> Bool 
prop_all_complete2 g = all (prop_complete . board) (devolveGame <$> extendGame g)

prop_all_disjoint2 :: Game -> Bool 
prop_all_disjoint2 g = all (prop_disjoint . board) (devolveGame <$> extendGame g)

return []
runTests :: IO Bool 
runTests = $quickCheckAll

main :: IO ()
main = void runTests
