{-# LANGUAGE DataKinds, OverloadedRecordDot #-}

module AbilitiesSpec (spec) where

import App.Character.Abilities (Abilities (..), AbilityProbabilities (..), mkAbilities)
import App.Character.Metalborn (Halfborn (..), Metal, Metalborn (..))
import App.RNG.Probability     (unProb)
import Control.Lens            ((^.))
import Data.Default            (Default (def))
import Data.Generics.Product   (HasPosition (position))
import GHC.Float               (int2Float)
import System.Random           (mkStdGen)
import Test.Hspec              (Spec, it, shouldBe)

----------------------------------------------------------------------------------------------------

spec ∷ Spec
spec = do
  let probs  = def @AbilityProbabilities
      volume = 100000
      outs   = map (evalState (mkAbilities probs) . mkStdGen) [0..volume]
      -- | MOE = Margin Of Error
      withinMOE (unProb → prob) out =
        let expected = (int2Float volume * prob)
            lo       = round (expected * 0.8) ∷ Int
            hi       = round (expected * 1.2) ∷ Int
         in out ∈ [lo..hi]
      withAtLeastOne mapper    = filter (not . null) $ map mapper outs
      spikesOut                = withAtLeastOne spikes
      medailOut                = withAtLeastOne medails
      outsWithAtLeastOneSpike  = length spikesOut
      outsWithAtLeastOneMedail = length medailOut
      outsMetalborn            = length $ filter isMetalborn outs
      outsTwinborn             = length $ filter isTwinborn outs
      spikesMetalsGrouped      = group . sort . join $ spikesOut
      medailMetalsGrouped      = group . sort . join $ medailOut
      outsWithGrenade          = length $ filter (^. position @2 . #grenade) outs

  it "should produce expected proportion of metalborn" $ do
    withinMOE probs.metalborn outsMetalborn `shouldBe` True

  it "should produce expected proportion of twinborn" $ do
    withinMOE (probs.metalborn * probs.twinborn) outsTwinborn `shouldBe` True

  it "should produce expected proportion of spikes" $ do
    withinMOE probs.spike outsWithAtLeastOneSpike `shouldBe` True
    let twoSpikeProb = probs.spike * probs.spike
        outsWithAtLeastTwoSpikes = length . filter ((2 ≤) . length) $ map spikes outs
    withinMOE twoSpikeProb outsWithAtLeastTwoSpikes `shouldBe` True

  it "should produce expected proportion of medaillions" $ do
    withinMOE probs.medail outsWithAtLeastOneMedail `shouldBe` True
    let twoMedailProb = probs.medail * probs.medail
        outsWithAtLeastTwoMedail = length . filter ((2 ≤) . length) $ map medails outs
    withinMOE twoMedailProb outsWithAtLeastTwoMedail `shouldBe` True

  it "should produce expected proportion of grenade users" $ do
    withinMOE probs.grenade outsWithGrenade `shouldBe` True

  it "should produce all 16 metals" $ do
    length spikesMetalsGrouped `shouldBe` 16
    length medailMetalsGrouped `shouldBe` 16

  it "should use the specified probabilities" $ do
    all (withinMOE (0.0625 * probs.spike) . length) spikesMetalsGrouped `shouldBe` True
    all (withinMOE (0.0625 * probs.medail) . length) medailMetalsGrouped `shouldBe` True

    True `shouldBe` True

isMetalborn ∷ Abilities → Bool
isMetalborn (Abilities mb _) = isJust mb

isTwinborn ∷ Abilities → Bool
isTwinborn (Abilities mb _) = case mb of
  Nothing                                → False
  Just (Twinborn {})                     → True
  Just (Halfborn (Mistborn (Just _)))    → True
  Just (Halfborn (Feruchemist (Just _))) → True
  _                                      → False

spikes ∷ Abilities → [Metal]
spikes (Abilities _ o) = (o ^. #spikedA) ++ (o ^. #spikedF)

medails ∷ Abilities → [Metal]
medails (Abilities _ o) = (o ^. #medailA) ++ (o ^. #medailF)
