{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, FlexibleContexts,
             NoMonomorphismRestriction #-}

module App.Character (Abilities (..), Character (..)) where

import App.InbornPower (InbornAllo, InbornFeru)
import App.Metal       (Metal)
import Data.Default    (Default (..))

----------------------------------------------------------------------------------------------------

data Character
  = Character
    { id        ∷ Text
    , name      ∷ Text
    , age       ∷ Int
    , gender    ∷ Gender
    , species   ∷ Species
    , abilities ∷ Abilities
    , lore      ∷ CharacterLore
    }

data Species = Human | Kandra | Koloss

data Gender = Male | Female | Other

data Abilities
  = Abilities
    { inbornA ∷ InbornAllo
    , inbornF ∷ InbornFeru
    , spikedA ∷ [Metal]
    , spikedF ∷ [Metal]
    , medailA ∷ [Metal]
    , medailF ∷ [Metal]
    , grenade ∷ Bool
    }
  deriving (Generic, Show)

instance Default Abilities where
  def = Abilities def def [] [] [] [] False

data CharacterLore
  = CharacterLore
    { job                ∷ Job
    , worldhopper        ∷ Bool
    , earlyLife          ∷ EarlyLife
    , crisis             ∷ Crisis
    , partners           ∷ PartnerInfo
    , abilitiesAnecdotes ∷ [AbilitiesAnecdotes]
    }

data Job = Constable | Scholar | BountyHunter

data EarlyLife = AllomancerFamily | Slums | Prodigy

data Crisis = TookDownTerroristOrg | LostFaithInHarmony

data PartnerInfo
  = NoPartner
  | OnePartner Partner
  | InTeam Team

data Partner = Partner

data Team = Team

data AbilitiesAnecdotes = AbilitiesAnecdotes
