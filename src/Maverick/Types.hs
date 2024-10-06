{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Maverick.Types where

import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)), FiniteBits)
import Data.Char (intToDigit)
import Data.Text qualified as T
import GHC.Show (showParen, showString)
import Numeric (showHex, showIntAtBase)
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

instance Show RankSet where
  showsPrec d r =
    showParen (d > 10) $ showString "RankSet 0b" . ((toString (render r)) ++)

instance Show RankCount where
  showsPrec d r =
    showParen (d > 10) $ showString "RankCount 0x" . ((toString (render r)) ++)

instance Show SuitSet where
  showsPrec d r =
    showParen (d > 10) $ showString "SuitSet 0b" . ((toString (render r)) ++)

instance Show SuitCount where
  showsPrec d r =
    showParen (d > 10) $ showString "SuitCount 0x" . ((toString (render r)) ++)

data Card = Card
  { cardRank :: !Rank,
    cardSuit :: !Suit,
    cardRankCount :: !RankCount,
    cardSuitSet :: !SuitSet,
    cardSuitCount :: !SuitCount
  }
  deriving (Show, Eq, Ord)

data Hand = Hand
  { handRankCount :: !RankCount,
    handSuitSet :: !SuitSet,
    handSuitCount :: !SuitCount
  }
  deriving (Show, Eq, Ord)

setMask :: SuitSet
setMask = SuitSet 0b11111111111111
{-# INLINE setMask #-}

countMask :: Word
countMask = 0xf
{-# INLINE countMask #-}

rankSet :: SuitSet -> RankSet
rankSet s =
  let SuitSet r =
        ( (setMask .&. s)
            .|. (setMask .&. (s `shiftR` 14))
            .|. (setMask .&. (s `shiftR` 28))
            .|. (setMask .&. (s `shiftR` 42))
        )
   in RankSet r
{-# INLINE rankSet #-}

getRankCount :: Rank -> RankCount -> Word
getRankCount r (RankCount s) = countMask .&. (s `shiftR` (fromEnum r * 4))
{-# INLINE getRankCount #-}

getSuitCount :: Suit -> SuitCount -> Word
getSuitCount r (SuitCount s) = countMask .&. (s `shiftR` (fromEnum r * 4))
{-# INLINE getSuitCount #-}

getSuitRankSet :: Suit -> SuitSet -> RankSet
getSuitRankSet s r = coerce $ (r `shiftR` (fromEnum s * 14)) .&. setMask
{-# INLINE getSuitRankSet #-}

rankCount :: Rank -> RankCount
rankCount r = RankCount (1 `shiftL` (4 * fromEnum r))
{-# INLINE rankCount #-}

suitSet :: Rank -> Suit -> SuitSet
suitSet Ace s = SuitSet ((1 `shiftL` 13 + 1) `shiftL` (14 * fromEnum s))
suitSet r s = SuitSet ((2 `shiftL` fromEnum r) `shiftL` (14 * fromEnum s))
{-# INLINE suitSet #-}

suitCount :: Suit -> SuitCount
suitCount r = SuitCount (1 `shiftL` (4 * fromEnum r))
{-# INLINE suitCount #-}

card :: Rank -> Suit -> Card
card r s = Card r s (rankCount r) (suitSet r s) (suitCount s)
{-# INLINE card #-}

hand :: Card -> Hand
hand c = Hand c.cardRankCount c.cardSuitSet c.cardSuitCount
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
    T.justifyRight 13 '0' . toText $ showHex s ""

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

instance Render Card where
  render c =
    -- T.intercalate
    --   " "
    --   [render r <> render s, render rs, render rc, render sc]
    render c.cardRank <> render c.cardSuit

instance Render Hand where
  render (Hand rs rc sc) =
    T.intercalate " " [render rs, render rc, render sc]

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

instance Semigroup Hand where
  (Hand r c s) <> (Hand r' c' s') = Hand (r <> r') (c <> c') (s <> s')
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

instance Monoid Hand where
  mempty = Hand mempty mempty mempty
  {-# INLINE mempty #-}

(<+) :: Hand -> Card -> Hand
(<+) h c =
  Hand
    (h.handRankCount <> c.cardRankCount)
    (h.handSuitSet <> c.cardSuitSet)
    (h.handSuitCount <> c.cardSuitCount)
{-# INLINE (<+) #-}

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
