module App.MetalbornNames (MetalbornName (..), MetalbornNameAndReason (..), metalbornNames) where

import App.InbornPower (InbornAllo (..), InbornFeru (..), ZeroOneOrEvery (..))
import App.Metal       (Metal (..))

----------------------------------------------------------------------------------------------------

-- https://www.17thshard.com/forum/topic/97725-twinborn-names/
data MetalbornName = Mistborn | FullFeruchemist | Coinshot | Lurcher | Rioter | Soother | Thug | Tineye | Smoker | Seeker | DuraluminGnat | AluminumGnat | Augur | Oracle | Nicroburst | Leecher | Pulser | Slider | Skimmer | Steelrunner | Sparker | Firesoul | Windwhisper | Brute | Archivist | Sentry | Spinner | Soulbearer | Gasper | Subsumer | Trueself | Connector | Bloodmaker | Pinnacle | EagleEye | Catcher | Monitor | Quickwit | Keeneye | Hefter | Sprinter | Sooner | Scrapper | Bruteblood | Marathoner | Scaler | Deader | Guardian | Navigator | Stalwart | Sharpshooter | Crasher | Swift | Shroud | Bigshot | Luckshot | Cloudtoucher | Copperkeep | Boiler | Ghostwalker | Shelter | Masker | Sentinel | Hazedodger | Metalmapper | Sleepless | Pulsewise | Stalker | Strongarm | Mastermind | Loudmouth | Zealot | Highroller | Instigator | Schemer | Cooler | Icon | Pacifier | Slick | Resolute | Puremind | Friendly | Metalbreaker | Ringer | Sapper | Gulper | Booster | BurstTicker | Enabler | Soulburst | Cohort | Chronicler | Vessel | Timeless | Introspect | Whimflitter | Foresight | Flicker | Charmed | Visionary | Plotter | Yearspanner | Chrysalis | Spotter | Blur | Assessor | Flashwit | Monument | Constant | Transcendent | Sated deriving
  ( Eq
  , Show
  )

-- | We want to know a character's metalborn names, and the precise
-- reason for each name
data MetalbornNameAndReason
  = MetalbornNameAndReason
    { reason ∷ Reason
    , name   ∷ MetalbornName
    }

data Reason
  = ReasonAllo InbornAllo
  | ReasonFeru InbornFeru
  | ReasonTwin InbornAllo InbornFeru

metalbornNames ∷ InbornAllo → InbornFeru → [MetalbornNameAndReason]
metalbornNames powerA powerF =
  catMaybes
    [ allomancerName powerA
    , feruchemistName powerF
    , twinbornName powerA powerF
    ]

allomancerName ∷ InbornAllo → Maybe MetalbornNameAndReason
allomancerName inbA@(InbornAllo power) =
  case power of
    Zero      → Nothing
    Every     → Just $ MetalbornNameAndReason (ReasonAllo inbA)   Mistborn
    One metal → Just $ MetalbornNameAndReason (ReasonAllo inbA) $ case metal of
      Steel     → Coinshot
      Iron      → Lurcher
      Zinc      → Rioter
      Brass     → Soother
      Pewter    → Thug
      Tin       → Tineye
      Copper    → Smoker
      Bronze    → Seeker
      Duralumin → DuraluminGnat
      Aluminum  → AluminumGnat
      Gold      → Augur
      Electrum  → Oracle
      Nicrosil  → Nicroburst
      Chromium  → Leecher
      Cadmium   → Pulser
      Bendalloy → Slider

feruchemistName ∷ InbornFeru → Maybe MetalbornNameAndReason
feruchemistName inbF@(InbornFeru power) =
  case power of
    Zero      → Nothing
    Every     → Just $ MetalbornNameAndReason (ReasonFeru inbF)   FullFeruchemist
    One metal → Just $ MetalbornNameAndReason (ReasonFeru inbF) $ case metal of
      Steel     → Steelrunner
      Iron      → Skimmer
      Zinc      → Sparker
      Brass     → Firesoul
      Pewter    → Windwhisper
      Tin       → Brute
      Copper    → Archivist
      Bronze    → Sentry
      Duralumin → Spinner
      Aluminum  → Soulbearer
      Gold      → Gasper
      Electrum  → Subsumer
      Nicrosil  → Trueself
      Chromium  → Connector
      Cadmium   → Bloodmaker
      Bendalloy → Pinnacle

twinbornName ∷ InbornAllo → InbornFeru → Maybe MetalbornNameAndReason
twinbornName inbA@(InbornAllo powerA) inbF@(InbornFeru powerF) =
  case (powerA, powerF) of
    (One metalA, One metalF) →
      let name = Just . MetalbornNameAndReason (ReasonTwin inbA inbF)
       in case (metalA, metalF) of
        (Tin, Tin)             → name EagleEye
        (Tin, Steel)           → name Catcher
        (Tin, Copper)          → name Monitor
        (Tin, Zinc)            → name Quickwit
        (Tin, Chromium)        → name Keeneye
        (Pewter, Pewter)       → name Hefter
        (Pewter, Steel)        → name Sprinter
        (Pewter, Zinc)         → name Sooner
        (Pewter, Chromium)     → name Scrapper
        (Pewter, Gold)         → name Bruteblood
        (Pewter, Cadmium)      → name Marathoner
        (Iron, Pewter)         → name Scaler
        (Iron, Iron)           → name Deader
        (Iron, Steel)          → name Guardian
        (Iron, Zinc)           → name Navigator
        (Iron, Gold)           → name Stalwart
        (Steel, Tin)           → name Sharpshooter
        (Steel, Iron)          → name Crasher
        (Steel, Steel)         → name Swift
        (Steel, Aluminum)      → name Shroud
        (Steel, Duralumin)     → name Bigshot
        (Steel, Chromium)      → name Luckshot
        (Steel, Cadmium)       → name Cloudtoucher
        (Copper, Copper)       → name Copperkeep
        (Copper, Bronze)       → name Shroud
        (Copper, Brass)        → name Boiler
        (Copper, Aluminum)     → name Ghostwalker
        (Copper, Duralumin)    → name Shelter
        (Copper, Chromium)     → name Masker
        (Bronze, Tin)          → name Sentinel
        (Bronze, Steel)        → name Hazedodger
        (Bronze, Copper)       → name Metalmapper
        (Bronze, Bronze)       → name Sleepless
        (Bronze, Zinc)         → name Pulsewise
        (Bronze, Bendalloy)    → name Stalker
        (Zinc, Pewter)         → name Strongarm
        (Zinc, Zinc)           → name Mastermind
        (Zinc, Aluminum)       → name Loudmouth
        (Zinc, Duralumin)      → name Zealot
        (Zinc, Chromium)       → name Highroller
        (Zinc, Electrum)       → name Instigator
        (Brass, Zinc)          → name Schemer
        (Brass, Brass)         → name Cooler
        (Brass, Aluminum)      → name Icon
        (Brass, Duralumin)     → name Pacifier
        (Brass, Chromium)      → name Slick
        (Brass, Electrum)      → name Resolute
        (Aluminum, Aluminum)   → name Puremind
        (Duralumin, Duralumin) → name Friendly
        (Chromium, Pewter)     → name Metalbreaker
        (Chromium, Chromium)   → name Ringer
        (Chromium, Nicrosil)   → name Sapper
        (Chromium, Bendalloy)  → name Gulper
        (Nicrosil, Pewter)     → name Booster
        (Nicrosil, Copper)     → name BurstTicker
        (Nicrosil, Duralumin)  → name Enabler
        (Nicrosil, Nicrosil)   → name Soulburst
        (Nicrosil, Electrum)   → name Cohort
        (Gold, Copper)         → name Chronicler
        (Gold, Aluminum)       → name Vessel
        (Gold, Gold)           → name Timeless
        (Gold, Electrum)       → name Introspect
        (Electrum, Iron)       → name Whimflitter
        (Electrum, Copper)     → name Foresight
        (Electrum, Zinc)       → name Flicker
        (Electrum, Chromium)   → name Charmed
        (Electrum, Electrum)   → name Visionary
        (Cadmium, Bronze)      → name Plotter
        (Cadmium, Gold)        → name Yearspanner
        (Cadmium, Cadmium)     → name Chrysalis
        (Bendalloy, Tin)       → name Spotter
        (Bendalloy, Steel)     → name Blur
        (Bendalloy, Copper)    → name Assessor
        (Bendalloy, Zinc)      → name Flashwit
        (Bendalloy, Aluminum)  → name Monument
        (Bendalloy, Gold)      → name Constant
        (Bendalloy, Electrum)  → name Transcendent
        (Bendalloy, Bendalloy) → name Sated
        _                      → Nothing

    _ → Nothing
