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
  , _row :: DeckRow
  , _player :: Int
  }
  deriving (Eq, Show, Ord)

makeLenses ''Origin


data Target = Opp | Self
  deriving (Show, Eq, Ord)

type CardEffect = Program CardEffectOp ()

data CardEffectOp a where
  AddDP :: Int -> CardEffectOp ()
  AddAP :: Int -> CardEffectOp ()
  AddCard :: Card -> Origin -> Target -> CardEffectOp ()
  Log :: String -> CardEffectOp ()
  AddTimer :: Int -> Bool -> CardEffect -> CardEffectOp ()
  GetOrigin :: CardEffectOp Origin
  Blocked :: CardEffectOp Bool
  Combo :: Int -> CardEffectOp ()
  Draw :: Origin -> CardEffectOp ()

addDP a = singleton $ AddDP a
addAP a = singleton $ AddAP a
addCard a b c = singleton $ AddCard a b c
logG a = singleton $ Log a
addTimer a b c = singleton $ AddTimer a b c
addBTimer a b = singleton $ AddTimer a True b
addNBTimer a b = singleton $ AddTimer a False b
getOrigin = singleton GetOrigin
blocked = singleton Blocked
combo a = singleton $ Combo a
draw a = singleton $ Draw a

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
      o <- getOrigin
      draw o
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
      o <- getOrigin
      addCard dmg11Card o Self
      blockedEff 2 (do
        logG $ "dmg 1 " ++ show (o ^. column)
        replicateM_ 1 (addCard dmgCard o Opp))
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
  , _cardEffect = do
    logG "combo test card"
    addNBTimer 6 $ return ()
    combo 6
  , _cardReqs = do
    combo <- comboCards 6
    return $ combo >= 2
  }

cantripTestCard :: Card
cantripTestCard = Card
  { _cardId = 7
  , _cardEffect = do
    logG "cantrip test card"
    addNBTimer 7 $ return ()
    addAP 1
  , _cardReqs = return True
  }

focus5Card :: Card
focus5Card = Card
  { _cardId = 8
  , _cardEffect = do
    logG "focus 5 card"
    o@(Origin col row p) <- getOrigin
    addCard dmg11Card o Self
    blockedEff 3 (do
          logG $ "dmg 7-3 " ++ show (o ^. column)
          replicateM_ 7 (addCard dmgCard (Origin col F p) Opp)
          replicateM_ 3 (addCard dmgCard (Origin col B p) Opp)
      )
  , _cardReqs = do
      focus <- focusCards
      return $ focus >= 5
  }

addFocus :: Card
addFocus = Card
  { _cardId = 9
  , _cardEffect = do
      logG "add focus"
      o <- getOrigin
      replicateM_ 3 $ addCard focusCard o Self
  , _cardReqs = return True
  }

focus3Card :: Card
focus3Card = Card
  { _cardId = 10
  , _cardEffect = do
      logG "focus 5 card"
      o@(Origin col row p) <- getOrigin
      addCard dmg11Card o Self
      blockedEff 3 (do
        logG $ "dmg 5-1 " ++ show (o ^. column)
        replicateM_ 5 (addCard dmgCard (Origin col F p) Opp)
        replicateM_ 1 (addCard dmgCard (Origin col B p) Opp)
        )
  , _cardReqs = do
      focus <- focusCards
      return $ focus >= 3
  }

block2Card :: Card
block2Card = Card
  { _cardId = 11
  , _cardEffect = do
      logG "block2Card"
      o@(Origin col row p) <- getOrigin
      addCard block2Card o Self
      addBTimer 2 $ return ()
      combo 11
  , _cardReqs = do
      combo <- comboCards 11
      return $ combo >= 2
  }

incCard :: Int -> Card
incCard n = Card
  { _cardId = 12
  , _cardEffect = do
      logG $ "inc card " ++ show n
      o@(Origin col row p) <- getOrigin
      addCard (incCard (n+1)) o Self
      blockedEff 3 (do
        logG $ "dmg " ++ show n ++ " " ++ show (o ^. column)
        replicateM_ n (addCard dmgCard (Origin col F p) Opp)
        )
  , _cardReqs = return True
  }

isDamage :: Card -> Bool
isDamage c = c ^. cardId == 1

isFocus :: Card -> Bool
isFocus c = c ^. cardId == 2

isCombo :: CardId -> Card -> Bool
isCombo cid c = c ^. cardId == cid

blockedEff :: Int -> CardEffect -> CardEffect
blockedEff t ce = do
  o <- getOrigin
  addCard dmg11Card o Self
  addNBTimer t (do
    b <- blocked
    if b
      then
        logG "(blocked) dmg"
      else
        ce
     )

drawCard :: Card -> String
drawCard c | c ^. cardId == 1 = " D"
drawCard c | c ^. cardId == 2 = " F"
drawCard c | c ^. cardId . to (length . show) == 1 = " " ++ show (c ^. cardId)
drawCard c | c ^. cardId . to (length . show) == 2 = show (c ^. cardId)
