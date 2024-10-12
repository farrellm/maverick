{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Maverick.Types where

import Data.Bits (Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.)), FiniteBits)
import Data.Char (intToDigit)
import Data.Text qualified as T
import GHC.Show (showParen, showString)
import Numeric (showHex, showIntAtBase, showOct)
import Text.Show (Show (showsPrec))

data Suit = Club | Diamond | Heart | Spade
  deriving (Show, Eq, Ord, Enum, Bounded)

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Enum, Bounded)

newtype RankSet = RankSet Word
  deriving (Eq, Ord, Bits, FiniteBits)

newtype RankCount = RankCount {unRankCount :: Word}
  deriving (Eq, Ord, Bits, FiniteBits)

newtype SuitSet = SuitSet {unSuitSet :: Word}
  deriving (Eq, Ord, Bits, FiniteBits)

newtype SuitCount = SuitCount {unSuitCount :: Word}
  deriving (Eq, Ord, Bits, FiniteBits)

newtype ComboCount = ComboCount {unComboCount :: Word}
  deriving (Eq, Ord, Bits, FiniteBits)

instance Show RankSet where
  showsPrec d r =
    showParen (d > 10) $ showString "RankSet 0b" . (toString (render r) ++)

instance Show RankCount where
  showsPrec d r =
    showParen (d > 10) $ showString "RankCount 0x" . (toString (render r) ++)

instance Show SuitSet where
  showsPrec d r =
    showParen (d > 10) $ showString "SuitSet 0b" . (toString (render r) ++)

instance Show SuitCount where
  showsPrec d r =
    showParen (d > 10) $ showString "SuitCount 0x" . (toString (render r) ++)

instance Show ComboCount where
  showsPrec d (ComboCount r) =
    showParen (d > 10) $ showString "ComboCount 0x" . showHex r

data Card = Card
  { cardRank :: !Rank,
    cardSuit :: !Suit,
    cardCount :: !ComboCount,
    cardSuitSet :: !SuitSet
  }
  deriving (Show, Eq, Ord)

data Hand = Hand
  { handCount :: !ComboCount,
    handSuitSet :: !SuitSet
  }
  deriving (Show, Eq, Ord)

setMask :: SuitSet
setMask = SuitSet 0b11111111111111
{-# INLINE setMask #-}

suitCountMask :: Word
suitCountMask = 0xf
{-# INLINE suitCountMask #-}

rankCountMask :: Word
rankCountMask = 0x7
{-# INLINE rankCountMask #-}

rankSet :: SuitSet -> RankSet
rankSet s =
  coerce $
    setMask
      .&. ( s
              .|. (s `unsafeShiftR` 14)
              .|. (s `unsafeShiftR` 28)
              .|. (s `unsafeShiftR` 42)
          )
{-# INLINE rankSet #-}

getRankCount :: Rank -> RankCount -> Word
getRankCount r (RankCount s) = rankCountMask .&. (s `unsafeShiftR` (fromEnum r * 3))
{-# INLINE getRankCount #-}

getSuitCount :: Suit -> SuitCount -> Word
getSuitCount r (SuitCount s) = suitCountMask .&. (s `unsafeShiftR` (fromEnum r * 4))
{-# INLINE getSuitCount #-}

getRankCount' :: Rank -> ComboCount -> Word
getRankCount' r = getRankCount r . coerce
{-# INLINE getRankCount' #-}

getSuitCount' :: Suit -> ComboCount -> Word
getSuitCount' r s = suitCountMask .&. (coerce s `unsafeShiftR` (fromEnum r * 4 + 39))
{-# INLINE getSuitCount' #-}

getSuitRankSet :: Suit -> SuitSet -> RankSet
getSuitRankSet s r = coerce $ (r `unsafeShiftR` (fromEnum s * 14)) .&. setMask
{-# INLINE getSuitRankSet #-}

rankCount :: Rank -> RankCount
rankCount r = RankCount (1 `unsafeShiftL` (3 * fromEnum r))
{-# INLINE rankCount #-}

suitSet :: Rank -> Suit -> SuitSet
suitSet Ace s = SuitSet ((1 `unsafeShiftL` 13 + 1) `unsafeShiftL` (14 * fromEnum s))
suitSet r s = SuitSet ((2 `unsafeShiftL` fromEnum r) `unsafeShiftL` (14 * fromEnum s))
{-# INLINE suitSet #-}

suitCount :: Suit -> SuitCount
suitCount r = SuitCount (1 `unsafeShiftL` (4 * fromEnum r))
{-# INLINE suitCount #-}

comboCount :: Rank -> Suit -> ComboCount
comboCount r s =
  let RankCount r' = rankCount r
      SuitCount s' = suitCount s
   in ComboCount $ (s' `unsafeShiftL` 39) .|. r'
{-# INLINE comboCount #-}

comboRankCount :: ComboCount -> RankCount
comboRankCount (ComboCount cc) = RankCount (0o7777777777777 .&. cc)
{-# INLINE comboRankCount #-}

comboSuitCount :: ComboCount -> SuitCount
comboSuitCount (ComboCount cc) = SuitCount (cc `unsafeShiftR` 39)
{-# INLINE comboSuitCount #-}

card :: Rank -> Suit -> Card
card r s = Card r s (comboCount r s) (suitSet r s)
{-# INLINE card #-}

hand :: Card -> Hand
hand c = Hand c.cardCount c.cardSuitSet
{-# INLINE hand #-}

class Render a where
  render :: a -> Text

instance Render Suit where
  render Club = "♣"
  render Diamond = "♦"
  render Heart = "♥"
  render Spade = "♠"

instance Render Rank where
  render Two = "2"
  render Three = "3"
  render Four = "4"
  render Five = "5"
  render Six = "6"
  render Seven = "7"
  render Eight = "8"
  render Nine = "9"
  render Ten = "T"
  render Jack = "J"
  render Queen = "Q"
  render King = "K"
  render Ace = "A"

instance Render RankSet where
  render (RankSet r) =
    T.justifyRight 14 '0' . toText $ showIntAtBase 2 intToDigit r ""

instance Render RankCount where
  render (RankCount s) =
    T.justifyRight 13 '0' . toText $ showOct s ""

instance Render SuitSet where
  render (SuitSet r) =
    T.intercalate "_"
      . T.chunksOf 14
      . T.justifyRight 56 '0'
      . toText
      $ showIntAtBase 2 intToDigit r ""

instance Render SuitCount where
  render (SuitCount s) =
    T.justifyRight 4 '0'
      . toText
      $ showHex s ""

instance Render ComboCount where
  render cc =
    T.intercalate
      " "
      [ --  toText (showHex (un cc :: Word) ""),
        render (comboSuitCount cc),
        render (comboRankCount cc)
      ]

instance Render Card where
  render c =
    -- T.intercalate
    --   " "
    --   [ render c.cardRank <> render c.cardSuit,
    --     render c.cardCount,
    --     render c.cardSuitSet
    --   ]
    render c.cardRank <> render c.cardSuit

instance Render Hand where
  render (Hand cc rs) = T.intercalate " " [render cc, render rs]

instance Semigroup RankSet where
  RankSet r <> RankSet r' = RankSet (r .|. r')
  {-# INLINE (<>) #-}

instance Semigroup RankCount where
  RankCount r <> RankCount r' = RankCount (r + r')
  {-# INLINE (<>) #-}

instance Semigroup SuitSet where
  SuitSet r <> SuitSet r' = SuitSet (r .|. r')
  {-# INLINE (<>) #-}

instance Semigroup SuitCount where
  SuitCount r <> SuitCount r' = SuitCount (r + r')
  {-# INLINE (<>) #-}

instance Semigroup ComboCount where
  ComboCount r <> ComboCount r' = ComboCount (r + r')
  {-# INLINE (<>) #-}

instance Semigroup Hand where
  (Hand r c) <> (Hand r' c') = Hand (r <> r') (c <> c')
  {-# INLINE (<>) #-}

instance Monoid RankSet where
  mempty = RankSet 0
  {-# INLINE mempty #-}

instance Monoid RankCount where
  mempty = RankCount 0
  {-# INLINE mempty #-}

instance Monoid SuitSet where
  mempty = SuitSet 0
  {-# INLINE mempty #-}

instance Monoid SuitCount where
  mempty = SuitCount 0
  {-# INLINE mempty #-}

instance Monoid ComboCount where
  mempty = ComboCount 0
  {-# INLINE mempty #-}

instance Monoid Hand where
  mempty = Hand mempty mempty
  {-# INLINE mempty #-}

(<+) :: Hand -> Card -> Hand
(<+) h c = Hand (h.handCount <> c.cardCount) (h.handSuitSet <> c.cardSuitSet)
{-# INLINE (<+) #-}

(<+^) :: (Applicative f) => f Hand -> f Card -> f Hand
(<+^) = liftA2 (<+)
{-# INLINE (<+^) #-}

data Score
  = High RankSet
  | OnePair Rank RankSet
  | TwoPair Rank Rank RankSet
  | Trip Rank RankSet
  | Straight Int
  | Flush RankSet
  | FullHouse Rank Rank
  | Quad Rank RankSet
  | StraightFlush Int
  deriving (Show, Eq, Ord)
