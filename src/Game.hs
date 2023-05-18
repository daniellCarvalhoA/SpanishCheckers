{-# LANGUAGE RecordWildCards  #-}

module Game (Game(..)
            , initialGame
            , LocalHistory
            , GlobalHistory
            , testGame
            , testGame2
            , State(..)
            , isGameOver
            , buildInitialGame
            ) where 

import Types ( Color(..), Status (..), Turn(Human), Assoc, Size, ComputerMove , Turn(..), Path, Cache)
import Board (Board(..), initialboard, showBoard, testBoard, testBoard2)
import qualified ComputerMoves as CM (CMoves(..), allMoves)
import Test.QuickCheck (Arbitrary(arbitrary))
import qualified HumanMoves as HM 
import Data.Sequence ( Seq (..) )
import Data.Word (Word8, Word32)
import Data.List.NonEmpty (NonEmpty)

type LocalHistory  = Seq (Either SimpleMove CJumpMove)
type GlobalHistory = [Either SimpleMove CJumpMove]
type SimpleMove = (Size, (Word8,Word8))
type CJumpMove  = (Size, ComputerMove)

data State = Start 
           | StepS (Size, Word8, NonEmpty (Word8,Word8))
           | StepJ (Size, Word8, Word8, Word32, Cache, NonEmpty Path)
           deriving (Eq, Show)

data Game = Game {
    status :: Status
  , assoc  :: Assoc
  , board  :: Board
  , cMoves :: CM.CMoves 
  , hMoves :: HM.HMoves
  , lhisto :: LocalHistory
  , ghisto :: GlobalHistory
  , state  :: State
} deriving (Eq)

isGameOver :: Game -> Bool 
isGameOver g = status g == GameOver

instance Show Game where 
  show Game{..} = show status <> "\n" <> show assoc  <> "\n" 
               <> showBoard assoc board <> "\n" <> show cMoves <> "\n" 
               <> show hMoves <> "\n" <> show lhisto <> "\n" 
               <> show ghisto <> "\n" <> show state

instance Arbitrary Game where
  arbitrary = do 
    brd <- arbitrary
    asc <- arbitrary
    return $ Game OnGoing asc brd (CM.allMoves (fst asc) brd) HM.NoHMoves Empty [] Start


buildInitialGame :: IO Game 
buildInitialGame = do 
  putStrLn "Type (W) for white or (B) for black"
  c <- getLine 
  case c of 
    "W" -> return $ Game OnGoing (Human,White) initialboard CM.NoCMoves 
                         (HM.allMoves Human initialboard) Empty [] Start 
    "B" -> return $ Game OnGoing (Computer, White) initialboard (CM.allMoves Computer initialboard)
                         HM.NoHMoves Empty [] Start
    _   -> putStrLn "Invalid input" >> buildInitialGame 


initialGame :: Game 
initialGame = Game OnGoing (Human, White) initialboard CM.NoCMoves (HM.allMoves Human initialboard) Empty [] Start

testGame :: Game 
testGame = Game OnGoing (Human, Black) testBoard CM.NoCMoves (HM.allMoves Human testBoard) Empty [] Start

testGame2 :: Game 
testGame2 = Game OnGoing (Human, White) testBoard2  CM.NoCMoves (HM.allMoves Human testBoard2) Empty [] Start
