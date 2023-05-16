{-# LANGUAGE RecordWildCards  #-}

module Game (Game(..), initialGame, LocalHistory, GlobalHistory, testGame) where 

import Types ( Color(White), Status (..), Turn(Human), Assoc, Size, ComputerMove, HumanMove , Turn(..))
import Board (Board(..), initialboard, showBoard, testBoard)
import qualified ComputerMoves as CM (CMoves(..), allMoves)
import Test.QuickCheck (Arbitrary(arbitrary))
import qualified HumanMoves as HM 
import Data.Sequence ( Seq (..) )
import Data.Word (Word8)

type LocalHistory  = Seq (Either SimpleMove CJumpMove)
type GlobalHistory = [Either SimpleMove CJumpMove]
type SimpleMove = (Size, (Word8,Word8))
type CJumpMove  = (Size, ComputerMove)
-- type HJumpMove  = (Size, HumanMove) 
data Game = Game {
    status :: Status
  , assoc  :: Assoc
  , board  :: Board
  , cMoves :: CM.CMoves 
  , hMoves :: HM.HMoves
  , lhisto :: LocalHistory
  , ghisto :: GlobalHistory
} deriving (Eq)

instance Show Game where 
  show Game{..} = show status <> "\n" <> show assoc  <> "\n" <> showBoard assoc board <> "\n" 
               <> show cMoves <> "\n" <> show hMoves <> "\n" <> show lhisto <> "\n" <> show ghisto


instance Arbitrary Game where
  arbitrary = do 
    brd <- arbitrary
    asc <- arbitrary
    return $ Game OnGoing asc brd (CM.allMoves (fst asc) brd) HM.NoHMoves Empty []

initialGame :: Game 
initialGame = Game OnGoing (Human, White) initialboard CM.NoCMoves HM.NoHMoves Empty []

testGame :: Game 
testGame = Game OnGoing (Computer,White) testBoard (CM.allMoves Computer testBoard) HM.NoHMoves Empty []

