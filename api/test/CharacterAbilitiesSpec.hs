{-# LANGUAGE OverloadedRecordDot #-}

module CharacterAbilitiesSpec (spec) where

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
      medallOut                = withAtLeastOne medalls
      outsWithAtLeastOneSpike  = length spikesOut
      outsWithAtLeastOneMedall = length medallOut
      outsMetalborn            = length $ filter isMetalborn outs
      outsTwinborn             = length $ filter isTwinborn outs
      spikesMetalsGrouped      = group . sort . join $ spikesOut
      medallMetalsGrouped      = group . sort . join $ medallOut
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

  it "should produce expected proportion of medalllions" $ do
    withinMOE probs.medall outsWithAtLeastOneMedall `shouldBe` True
    let twoMedallProb = probs.medall * probs.medall
        outsWithAtLeastTwoMedall = length . filter ((2 ≤) . length) $ map medalls outs
    withinMOE twoMedallProb outsWithAtLeastTwoMedall `shouldBe` True

  it "should produce expected proportion of grenade users" $ do
    withinMOE probs.grenade outsWithGrenade `shouldBe` True

  it "should produce all 16 metals" $ do
    length spikesMetalsGrouped `shouldBe` 16
    length medallMetalsGrouped `shouldBe` 16

  it "should use the specified probabilities" $ do
    all (withinMOE (0.0625 * probs.spike) . length) spikesMetalsGrouped `shouldBe` True
    all (withinMOE (0.0625 * probs.medall) . length) medallMetalsGrouped `shouldBe` True

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

medalls ∷ Abilities → [Metal]
medalls (Abilities _ o) = (o ^. #medallA) ++ (o ^. #medallF)
