{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs           #-}

module Main where 

import Test.QuickCheck (quickCheck)
import Test.QuickCheck.All       (quickCheckAll)
import Board                     (Board(..))
import Data.Bits                 (Bits(..))
import Data.Functor              (void,(<&>))
import Data.Function             ((&))
import Data.List.NonEmpty        (NonEmpty(..))
import Data.Semigroup            (sconcat)
-- import Game                      (Spiel(..), HC(..), Nat(..))
-- import AlphaBeta                 (spielerweitern, zuruckkehren)
import Types                     (Turn(..))
import Game
import qualified Board as B 
import AI 
import JumpMoves
import Data.List (sort,sortOn) 
import Data.Word 
import SimpleMoves 
import Debug.Trace 

prop_disjoint :: B.Board -> Bool 
prop_disjoint B.Board{..} = 
     all ((== 0) . (empties   .&.)) [uppawns,downpawns,upkings,downkings]
  && all ((== 0) . (uppawns   .&.)) [downpawns,upkings,downkings]
  && all ((== 0) . (downpawns .&.)) [upkings,downkings]
  && upkings .&. downkings == 0
  where empties = complement $ uppawns .|. downpawns .|. upkings .|. downkings .|. mask

prop_complete :: B.Board -> Bool
prop_complete b@B.Board{..} = 
  (B.empties b .|. uppawns .|. downpawns .|. upkings .|. downkings .|. mask ) == maxBound

-- prop_iso :: B.Game 'B.Simulated -> Bool
-- prop_iso o@B.Over{} = True
-- prop_iso g@B.AI{}   = B.extendGames g <&> B.devolveGame & allEqual

apply :: (a -> a) -> Int -> a -> a 
apply f 1 x = f x 
apply f n x = f $ apply f (n - 1) x 

applyL :: (a -> NonEmpty a) -> Int -> a -> NonEmpty a 
applyL f 1 x = f x 
applyL f n x = (sconcat . fmap f) $ applyL f (n - 1) x 

-- prop_niso :: B.Game 'B.Simulated -> Bool
-- prop_niso o@B.Over{} = True
-- prop_niso g@B.AI{}   = applyL B.extendGames 3 g <&> apply B.devolveGame 3 & allEqual

allEqual :: Eq a => NonEmpty a -> Bool
allEqual  (h :| hs) = all (== h) hs

prop_all_disjoint :: Game 'Simulated -> Bool
prop_all_disjoint g  =
  all (prop_disjoint . getboard
      ) $ extendGames g


prop_all_complete :: Game 'Simulated -> Bool
prop_all_complete g =
  all (prop_complete . getboard
      ) $ extendGames g

prop_simples :: Board -> Turn -> Bool 
prop_simples board turn = 
 sortOn snd (allsimples board turn) == sortOn snd (allCSimplePawnMoves board turn)

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = void runTests
