{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE StandaloneDeriving     #-}

module Game ( Match(..)  
            , Play(..) 
            , Game(..)
            , GameType(..)
            , SG(..)
            , Dimensions(..)
            , History(..)
            , testGamey
            , Vec(..)
            , FirstMove
            , testGamex
            , getboard
            , getturn
            , allcmoves
            , undo
            , goback
            , redo
            , allhmoves
            , nforward
            , nullHistory
            , initialGame
            , testGame
            , testGame2
            , testGameab
            , testGameab2
            , startMatch
            , getdimensionsfromenv
            ) where

import Board 
import Types 
import SimpleMoves 
import JumpMoves 
import Data.Maybe         (fromJust)
import Data.Monoid        (First(..))
import Data.List.NonEmpty (nonEmpty)
import Data.Kind          (Type)
import Data.Tuple.Extra   (both)
import Test.QuickCheck    hiding (Result(..))
import qualified Graphics.Gloss.Interface.Environment as E 
import Debug.Trace
-------------------------
-- Dimensions of the window/cell etc, needed for rendering 

data Dimensions = Dimensions {
    screendimensions :: (Int,Int) 
  , windowdimensions :: (Int,Int)
  , cellsize         :: Float 
  , translation      :: (Int,Int) -- denotes where to center the window on the screen
  } deriving Show

getdimensionsfromenv :: IO Dimensions 
getdimensionsfromenv = do 
  screensize <- E.getScreenSize 
  let windim  = both width screensize 
      transl  = (negate $ fst windim `div` 2, negate $ snd windim `div` 2)
      celldim = fromIntegral (3 * uncurry min windim) / 32 
  return $ Dimensions screensize windim celldim transl 

width :: Int -> Int 
width w = 2 * w `div` 3

-------------------
-- Denotes The game history so that the user can undo or redo moves 

data Nat = Zero | Succ Nat 

data Vec (n :: Nat) (a :: Type) where
  Nil  :: Vec 'Zero a 
  Cons :: !a -> !(Vec n a) -> Vec ('Succ n) a 

deriving instance Show a => Show (Vec n a) 

data History n m = ZipVec (Vec n Move) (Vec m Move)
  deriving Show

nullHistory :: History 'Zero 'Zero 
nullHistory = ZipVec Nil Nil 

nforward :: Move -> History n m -> History ('Succ n) m 
nforward move (ZipVec v u) = ZipVec (Cons move v) u 

rforward :: History n ('Succ m) -> History ('Succ n) m
rforward (ZipVec v (Cons x u)) = ZipVec (Cons x v) u 

backward :: History ('Succ n) m -> History n ('Succ m)
backward (ZipVec (Cons x v) u) = ZipVec v (Cons x u)

----------------------------
---------- Match Type

data Play where
  Play :: { history :: History n m 
          , game    :: Game g 
          } -> Play

deriving instance Show Play

data Match where 
  Menu  :: { dims  :: Dimensions } -> Match 
  Match :: { dims  :: Dimensions
           , color :: Color
           , play  :: Play
           } -> Match 

deriving instance Show Match

startMatch :: Color -> Dimensions -> Match
startMatch White ds = Match ds White 
                    $ Play nullHistory 
                    $ HS initialboard (Start $ fromJust $ allhmoves initialboard)
startMatch Black ds = Match ds Black 
                    $ Play nullHistory 
                    $ AI Computer initialboard (fromJust $ allcmoves initialboard Computer) (First Nothing)

-------------------------
-- Game Type 

type FirstMove = First Move

data GameType = Real | Simulated 

type family ChangeTurn (g :: GameType) = r | r -> g where
  ChangeTurn 'Real = 'Simulated
  ChangeTurn 'Simulated = 'Real

data SG (g :: GameType) where
  SR :: SG 'Real 
  SS :: FirstMove -> SG 'Simulated

deriving instance Show (SG g) 
deriving instance Eq   (SG g) 

data Game (g :: GameType) where
  Over :: SG g -> Result -> Turn -> Board -> Game g 
  AI   :: Turn -> Board -> CMoves -> FirstMove -> Game 'Simulated 
  HS   :: Board -> IMove -> Game 'Real 

instance Arbitrary (Game 'Simulated) where
  arbitrary = do 
     board <- arbitrary
     turn  <- arbitrary 
     return $ case allcmoves board turn of 
                Nothing -> Over (SS $ First Nothing) Loss turn board 
                Just mv -> AI turn board mv (First Nothing)

deriving instance Eq (Game g) 
deriving instance Show (Game g)

getboard :: Game 'Simulated -> Board
getboard (Over _ _ _ board) = board 
getboard (AI _ board _ _)   = board

getturn :: Game 'Simulated -> Turn 
getturn (Over _ _ turn _) = turn 
getturn (AI turn _ _ _)   = turn


initialGame :: Game 'Simulated
initialGame =  AI Computer initialboard (fromJust $ allcmoves initialboard Computer) (First Nothing)

testGamex :: Game 'Simulated 
testGamex = AI Computer testx (fromJust $ allcmoves testx Computer) (First Nothing) 

testGamey :: Game 'Simulated 
testGamey = AI Computer testy (fromJust $ allcmoves testy Computer) (First Nothing)

testGame :: Game 'Simulated 
testGame = AI Computer test4 (fromJust $ allcmoves test4 Computer) (First Nothing) 

testGame2 :: Game 'Simulated 
testGame2 = AI Computer test5 (fromJust $ allcmoves test5 Computer) (First Nothing)

testGameab :: Game 'Simulated 
testGameab = AI Computer testab (fromJust $ allcmoves testab Computer) (First Nothing)

testGameab2 :: Game 'Simulated 
testGameab2 = AI Computer testab2 (fromJust $ allcmoves testab2 Computer) (First Nothing)


----------------------
-- All Computer Moves 

allcmoves :: Board -> Turn -> Maybe CMoves
allcmoves board turn =
  let jmoves = snd $ allckingjumps board turn `append` allcpawnjumps board turn 
      kmoves = allCSimpleKingMoves board turn 
      smoves = allsimples board turn 
  in case jmoves of 
     [] -> SCM <$> nonEmpty (kmoves <> smoves)
     xs -> JCM <$> nonEmpty xs 

allhmoves :: Board -> Maybe HMoves
allhmoves board =
  let jmoves = snd $ allhkingjumps board `append` allhpawnjumps board 
      kmoves = allHSimpleKingMoves board Human
      smoves = allHSimplePawnMoves board Human 
  in case jmoves of 
     [] -> SHM <$> nonEmpty (smoves <> kmoves)
     xs -> JHM <$> nonEmpty xs 

---------------------------------
-- Devolve Game 

undo :: (History ('Succ ('Succ n)) m, Game 'Real) -> (History n ('Succ ('Succ m)), Game 'Real) 
undo = goback . goback

redo :: (History n ('Succ ('Succ m)), Game 'Real) -> (History ('Succ ('Succ n)) m, Game 'Real)
redo = goforward . goforward 

goforward :: (History n ('Succ m), Game gt) -> (History ('Succ n) m, Game (ChangeTurn gt))
goforward (history, game) = 
  case game of 
    HS board _ -> 
      case history of 
        zv@(ZipVec _ (Cons (Left sm) _)) -> 
          let newboard = mkSimpleMove sm board Human 
              newmoves = allcmoves newboard Computer 
          in case newmoves of 
              Nothing -> (rforward zv, Over (SS mempty) Loss Computer newboard)
              Just mv -> (rforward zv, AI Computer newboard mv mempty)
        zv@(ZipVec _ (Cons (Right am) _)) -> 
          let newboard = mkJumpMove board am Human 
              newmoves = allcmoves newboard Computer
          in case newmoves of 
              Nothing -> (rforward zv, Over (SS mempty) Loss Computer newboard)
              Just mv -> (rforward zv, AI Computer newboard mv mempty) 
    AI _ board _ _ -> 
      case history of 
        zv@(ZipVec _ (Cons (Left sm) _)) -> 
          let newboard = mkSimpleMove sm board Computer
              newmoves = allhmoves newboard
          in case newmoves of 
              Nothing -> (rforward zv, Over SR Loss Human newboard)
              Just mv -> (rforward zv, HS newboard (Start mv))
        zv@(ZipVec _ (Cons (Right am) _)) -> 
          let newboard = mkJumpMove board am Computer 
              newmoves = allhmoves newboard 
          in case newmoves of 
              Nothing -> (rforward zv, Over SR Loss Human newboard)
              Just mv -> (rforward zv, HS newboard (Start mv))
    Over _ _ _ _ -> error "this should never happen" 
        
     

goback :: (History ('Succ n) m, Game gt) -> (History n ('Succ m), Game (ChangeTurn gt))
goback (history, game) = traceShow history $ 
  case game of 
    HS board _        -> 
      case history of 
        zv@(ZipVec (Cons (Left sm) _) _) -> 
          let newboard = rvSimpleMove sm board Human
              newmoves = allcmoves newboard Computer
          in (backward zv, AI Computer newboard (fromJust newmoves) mempty)
        zv@(ZipVec (Cons (Right am) _) _) ->
          let newboard = rvJumpMove am board Human
              newmoves = allcmoves newboard Computer
          in (backward zv, AI Computer newboard (fromJust newmoves) mempty)
    AI _ board _ _ -> 
      case history of 
        zv@(ZipVec (Cons (Left sm) _) _) -> 
          let newboard = rvSimpleMove sm board Computer
              newmoves = allhmoves newboard
          in (backward zv, HS newboard $ Start $ fromJust newmoves)
        zv@(ZipVec (Cons (Right am) _) _) ->
          let newboard = rvJumpMove am board Computer
              newmoves = allhmoves newboard 
          in (backward zv, HS newboard $ Start $ fromJust newmoves)
    Over SR _ _ board      -> 
      case history of 
        zv@(ZipVec (Cons (Left sm) _) _) -> 
          let newboard = rvSimpleMove sm board Human
              newmoves = allcmoves newboard Computer 
          in (backward zv, AI Computer newboard (fromJust newmoves) mempty)
        zv@(ZipVec (Cons (Right am) _) _) ->
          let newboard = rvJumpMove am board Human
              newmoves = allcmoves newboard Computer
          in (backward zv, AI Computer newboard (fromJust newmoves) mempty)
    Over (SS _) _ _ board  -> 
      case history of 
        zv@(ZipVec (Cons (Left sm) _) _) -> 
          let newboard = rvSimpleMove sm board Computer
              newmoves = allhmoves newboard
          in (backward zv, HS newboard $ Start $ fromJust newmoves)
        zv@(ZipVec (Cons (Right am) _) _) ->
          let newboard = rvJumpMove am board Computer 
              newmoves = allhmoves newboard 
          in (backward zv, HS newboard $ Start $ fromJust newmoves)

