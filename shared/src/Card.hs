{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Card
  where

import Control.Monad
import Control.Monad.Operational
import Control.Lens

type CardId = Int

data DeckColumn = L | M | R
  deriving (Show, Eq, Ord)

data DeckRow = F | B
  deriving (Show, Eq, Ord)

data Origin = Origin
  { _column :: DeckColumn
  , _player :: Int
  }

makeLenses ''Origin

type CardEffect = Program CardEffectOp ()

data CardEffectOp a where
  AddDP :: Int -> CardEffectOp ()
  AddAP :: Int -> CardEffectOp ()
  AddCard :: Card -> DeckColumn -> CardEffectOp ()
  Log :: String -> CardEffectOp ()
  AddTimer :: Int -> Bool -> CardEffect -> CardEffectOp ()
  GetOrigin :: CardEffectOp Origin
  Blocked :: CardEffectOp Bool

addDP a = singleton $ AddDP a
addAP a = singleton $ AddAP a
addCard a b = singleton $ AddCard a b
logG a = singleton $ Log a
addTimer a b c = singleton $ AddTimer a b c
addBTimer a b = singleton $ AddTimer a True b
addNBTimer a b = singleton $ AddTimer a False b
getOrigin = singleton GetOrigin
blocked = singleton Blocked

type CardReq = Program CardReqOp Bool

data CardReqOp a where
  FocusCards :: CardReqOp Int
  ComboCards :: CardId -> CardReqOp Int

focusCards = singleton FocusCards
comboCards a = singleton $ ComboCards a

data Card = Card
  { _cardId :: CardId
  , _cardEffect :: CardEffect
  , _cardReqs :: CardReq
  }

makeLenses ''Card

dmgCard :: Card
dmgCard = Card
  { _cardId = 1
  , _cardEffect = do
      logG "activate dmg card"
      addDP 1
  , _cardReqs = return True
  }

focusCard :: Card
focusCard = Card
  { _cardId = 2
  , _cardEffect =
      logG "activate focus card"
  , _cardReqs = return True
  }

dmg11Card :: Card
dmg11Card = Card
  { _cardId = 3
  , _cardEffect = do
      logG "queue 1 dmg timer 2"
      addTimer 2 False $ do
        b <- blocked
        if b
          then
            logG "(blocked) dmg 1"
          else do
            o <- getOrigin
            logG $ "dmg 1 " ++ show (o ^. column)
            replicateM_ 1 (addCard dmgCard (o ^. column))
  , _cardReqs = return True
  }

blockCard :: Card
blockCard = Card
  { _cardId = 4
  , _cardEffect = do
    logG "refresh block 2"
    addTimer 2 True $ return ()
  , _cardReqs = return True
  }

focusTestCard :: Card
focusTestCard = Card
  { _cardId = 5
  , _cardEffect =
      logG "focus test card"
  , _cardReqs = do
      focus <- focusCards
      return $ focus >= 1
  }

comboTestCard :: Card
comboTestCard = Card
  { _cardId = 6
  , _cardEffect =
    logG "combo test card"
  , _cardReqs = do
    combo <- comboCards 6
    return $ combo >= 2
  }

cantripTestCard :: Card
cantripTestCard = Card
  { _cardId = 7
  , _cardEffect = do
    logG "cantrip test card"
    addNBTimer 20 $ return ()
    addAP 1
  , _cardReqs = return True
  }

isFocus :: Card -> Bool
isFocus c = c ^. cardId == 2

isCombo :: CardId -> Card -> Bool
isCombo cid c = c ^. cardId == cid

drawCard :: Card -> String
drawCard c | c ^. cardId == 1 = "D"
drawCard c | c ^. cardId == 2 = "F"
drawCard c = show $ c ^. cardId
