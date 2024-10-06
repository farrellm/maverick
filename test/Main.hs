module Main (main) where

import Maverick.Score
import Maverick.Types
import Test.Hspec

main :: IO ()
main = hspec $ do
  let c1 =
        [ (Three, Spade),
          (Four, Spade),
          (Five, Spade),
          (Seven, Club),
          (Eight, Club)
        ]
      h1 = [(Ace, Diamond), (Nine, Diamond)]
      h2 = [(Ace, Diamond), (Eight, Diamond)]
      h3 = [(Seven, Diamond), (Eight, Diamond)]
      h4 = [(Eight, Spade), (Eight, Diamond)]
      h5 = [(Ace, Diamond), (Two, Diamond)]
      h6 = [(Six, Diamond), (Nine, Diamond)]
      h7 = [(Six, Spade), (Ace, Spade)]
      h8 = [(Six, Spade), (Seven, Spade)]
  describe "community 1" $ do
    it "hand 1: high card" $ do
      scoreHand (c1 ++ h1) `shouldBe` High (RankSet 0b1000011101000)
    it "hand 2: one pair" $ do
      scoreHand (c1 ++ h2) `shouldBe` OnePair Eight (RankSet 0b1000000101000)
    it "hand 3: two pair" $ do
      scoreHand (c1 ++ h3) `shouldBe` TwoPair Eight Seven (RankSet 0b0000000001000)
    it "hand 4: three of a kind" $ do
      scoreHand (c1 ++ h4) `shouldBe` Trip Eight (RankSet 0b0000000101000)
    it "hand 5: straight 0" $ do
      scoreHand (c1 ++ h5) `shouldBe` Straight 0
    it "hand 6: straight 5" $ do
      scoreHand (c1 ++ h6) `shouldBe` Straight 4
    it "hand 7: flush" $ do
      scoreHand (c1 ++ h7) `shouldBe` Flush (RankSet 0b1000000011110)
    it "hand 8: straight flush" $ do
      scoreHand (c1 ++ h8) `shouldBe` StraightFlush 2

scoreHand :: [(Rank, Suit)] -> Score
scoreHand = score . foldMap' (hand . uncurry card)
