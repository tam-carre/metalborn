module App.Character.Metalborn
  ( Ferring (..)
  , Halfborn (..)
  , Metal (..)
  , Metalborn (..)
  , Misting (..)
  , Singleborn (..)
  , Twinborn (..)
  , ferringMetal
  , mistingMetal
  , mkTwinborn
  , singlebornMetal
  ) where

import App.RNG.Rand  (randomEnum, randomEnumR)
import System.Random (Random (..))

----------------------------------------------------------------------------------------------------

data Metal = Iron | Steel | Tin | Pewter | Zinc | Brass | Copper | Bronze | Cadmium | Bendalloy | Gold | Electrum | Chromium | Nicrosil | Aluminum | Duralumin deriving
  ( Bounded
  , Enum
  , Eq
  , Generic
  , Ord
  , Show
  )

instance Random Metal where
  randomR = randomEnumR
  random  = randomEnum

data Metalborn
  = Singleborn Singleborn
  | Twinborn Misting Ferring (Maybe Twinborn)
  | Halfborn Halfborn
  | Fullborn
  deriving (Eq, Generic, Show)

data Singleborn
  = Misting Misting
  | Ferring Ferring
  deriving (Eq, Generic, Show)

data Halfborn
  = Mistborn (Maybe Ferring)
  | Feruchemist (Maybe Misting)
  deriving (Eq, Generic, Show)

data Misting = Coinshot | Lurcher | Rioter | Soother | Thug | Tineye | Smoker | Seeker | DuraluminGnat | AluminumGnat | Augur | Oracle | Nicroburst | Leecher | Pulser | Slider deriving
  ( Bounded
  , Enum
  , Eq
  , Generic
  , Ord
  , Show
  )

instance Random Misting where
  randomR = randomEnumR
  random  = randomEnum

data Ferring = Skimmer | Steelrunner | Sparker | Firesoul | Windwhisper | Brute | Archivist | Sentry | Spinner | Soulbearer | Gasper | Subsumer | Trueself | Connector | Bloodmaker | Pinnacle deriving
  ( Bounded
  , Enum
  , Eq
  , Generic
  , Ord
  , Show
  )

instance Random Ferring where
  randomR = randomEnumR
  random  = randomEnum

-- https://www.17thshard.com/forum/topic/97725-twinborn-names/
data Twinborn = EagleEye | Catcher | Monitor | Quickwit | Keeneye | Hefter | Sprinter | Sooner | Scrapper | Bruteblood | Marathoner | Scaler | Deader | Guardian | Navigator | Stalwart | Sharpshooter | Crasher | Swift | Shroud | Bigshot | Luckshot | Cloudtoucher | Copperkeep | Boiler | Ghostwalker | Shelter | Masker | Sentinel | Hazedodger | Metalmapper | Sleepless | Pulsewise | Stalker | Strongarm | Mastermind | Loudmouth | Zealot | Highroller | Instigator | Schemer | Cooler | Icon | Pacifier | Slick | Resolute | Puremind | Friendly | Metalbreaker | Ringer | Sapper | Gulper | Booster | BurstTicker | Enabler | Soulburst | Cohort | Chronicler | Vessel | Timeless | Introspect | Whimflitter | Foresight | Flicker | Charmed | Visionary | Plotter | Yearspanner | Chrysalis | Spotter | Blur | Assessor | Flashwit | Monument | Constant | Transcendent | Sated deriving
  ( Eq
  , Generic
  , Show
  )

mkTwinborn ∷ Misting → Ferring → Metalborn
mkTwinborn misting ferring = Twinborn misting ferring $ twinborn misting ferring

singlebornMetal ∷ Singleborn → Metal
singlebornMetal = \case
  Misting m → mistingMetal m
  Ferring f → ferringMetal f

mistingMetal ∷ Misting → Metal
mistingMetal = \case
  Coinshot      → Steel
  Lurcher       → Iron
  Rioter        → Zinc
  Soother       → Brass
  Thug          → Pewter
  Tineye        → Tin
  Smoker        → Copper
  Seeker        → Bronze
  DuraluminGnat → Duralumin
  AluminumGnat  → Aluminum
  Augur         → Gold
  Oracle        → Electrum
  Nicroburst    → Nicrosil
  Leecher       → Chromium
  Pulser        → Cadmium
  Slider        → Bendalloy

ferringMetal ∷ Ferring → Metal
ferringMetal = \case
  Steelrunner → Steel
  Skimmer     → Iron
  Sparker     → Zinc
  Firesoul    → Brass
  Windwhisper → Tin
  Brute       → Pewter
  Archivist   → Copper
  Sentry      → Bronze
  Spinner     → Chromium
  Connector   → Duralumin
  Soulbearer  → Nicrosil
  Gasper      → Cadmium
  Subsumer    → Bendalloy
  Trueself    → Aluminum
  Bloodmaker  → Gold
  Pinnacle    → Electrum

twinborn ∷ Misting → Ferring → Maybe Twinborn
twinborn misting ferring = case (misting, ferring) of
  (Tineye, Windwhisper)      → Just EagleEye
  (Tineye, Steelrunner)      → Just Catcher
  (Tineye, Archivist)        → Just Monitor
  (Tineye, Sparker)          → Just Quickwit
  (Tineye, Spinner)          → Just Keeneye
  (Thug, Brute)              → Just Hefter
  (Thug, Steelrunner)        → Just Sprinter
  (Thug, Sparker)            → Just Sooner
  (Thug, Spinner)            → Just Scrapper
  (Thug, Bloodmaker)         → Just Bruteblood
  (Thug, Gasper)             → Just Marathoner
  (Lurcher, Brute)           → Just Scaler
  (Lurcher, Skimmer)         → Just Deader
  (Lurcher, Steelrunner)     → Just Guardian
  (Lurcher, Sparker)         → Just Navigator
  (Lurcher, Bloodmaker)      → Just Stalwart
  (Coinshot, Windwhisper)    → Just Sharpshooter
  (Coinshot, Skimmer)        → Just Crasher
  (Coinshot, Steelrunner)    → Just Swift
  (Coinshot, Trueself)       → Just Shroud
  (Coinshot, Connector)      → Just Bigshot
  (Coinshot, Spinner)        → Just Luckshot
  (Coinshot, Gasper)         → Just Cloudtoucher
  (Smoker, Archivist)        → Just Copperkeep
  (Smoker, Sentry)           → Just Shroud
  (Smoker, Firesoul)         → Just Boiler
  (Smoker, Trueself)         → Just Ghostwalker
  (Smoker, Connector)        → Just Shelter
  (Smoker, Spinner)          → Just Masker
  (Seeker, Windwhisper)      → Just Sentinel
  (Seeker, Steelrunner)      → Just Hazedodger
  (Seeker, Archivist)        → Just Metalmapper
  (Seeker, Sentry)           → Just Sleepless
  (Seeker, Sparker)          → Just Pulsewise
  (Seeker, Subsumer)         → Just Stalker
  (Rioter, Brute)            → Just Strongarm
  (Rioter, Sparker)          → Just Mastermind
  (Rioter, Trueself)         → Just Loudmouth
  (Rioter, Connector)        → Just Zealot
  (Rioter, Spinner)          → Just Highroller
  (Rioter, Pinnacle)         → Just Instigator
  (Soother, Sparker)         → Just Schemer
  (Soother, Firesoul)        → Just Cooler
  (Soother, Trueself)        → Just Icon
  (Soother, Connector)       → Just Pacifier
  (Soother, Spinner)         → Just Slick
  (Soother, Pinnacle)        → Just Resolute
  (AluminumGnat, Trueself)   → Just Puremind
  (DuraluminGnat, Connector) → Just Friendly
  (Leecher, Brute)           → Just Metalbreaker
  (Leecher, Spinner)         → Just Ringer
  (Leecher, Soulbearer)      → Just Sapper
  (Leecher, Subsumer)        → Just Gulper
  (Nicroburst, Brute)        → Just Booster
  (Nicroburst, Archivist)    → Just BurstTicker
  (Nicroburst, Connector)    → Just Enabler
  (Nicroburst, Soulbearer)   → Just Soulburst
  (Nicroburst, Pinnacle)     → Just Cohort
  (Augur, Archivist)         → Just Chronicler
  (Augur, Trueself)          → Just Vessel
  (Augur, Bloodmaker)        → Just Timeless
  (Augur, Pinnacle)          → Just Introspect
  (Oracle, Skimmer)          → Just Whimflitter
  (Oracle, Archivist)        → Just Foresight
  (Oracle, Sparker)          → Just Flicker
  (Oracle, Spinner)          → Just Charmed
  (Oracle, Pinnacle)         → Just Visionary
  (Pulser, Sentry)           → Just Plotter
  (Pulser, Bloodmaker)       → Just Yearspanner
  (Pulser, Gasper)           → Just Chrysalis
  (Slider, Windwhisper)      → Just Spotter
  (Slider, Steelrunner)      → Just Blur
  (Slider, Archivist)        → Just Assessor
  (Slider, Sparker)          → Just Flashwit
  (Slider, Trueself)         → Just Monument
  (Slider, Bloodmaker)       → Just Constant
  (Slider, Pinnacle)         → Just Transcendent
  (Slider, Subsumer)         → Just Sated
  _                          → Nothing

