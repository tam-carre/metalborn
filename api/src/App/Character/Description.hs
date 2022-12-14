{-# LANGUAGE OverloadedLists, TemplateHaskell #-}

module App.Character.Description (DescriptionBlock (..), describeAbilities, seed) where

import App.Character.Abilities (Abilities (Abilities), AbilitiesObtained (AbilitiesObtained))
import App.Character.Metalborn (Ferring (..), Halfborn (..), Metal, Metalborn (..), Misting (..),
                                Singleborn (..), Twinborn (..), ferringMetal, feruchemicalAttribute,
                                mistingMetal)
import App.Character.Name      (Name (..))
import App.Gender              (Gender (Male), GenderNeutral, applyGender, their, them, themselves,
                                they, txt, uncapitalize, were)
import App.RNG.Rand            (Rand, randEl)
import Data.List               (singleton)
import Relude.Unsafe           qualified as Unsafe
import Servant.Elm             (defaultOptions, deriveBoth)
import System.Random           (StdGen, mkStdGen)

----------------------------------------------------------------------------------------------------

data DescriptionBlock
  = AllomancyBlock Text
  | FeruchemyBlock Text
  | TwinbornBlock Text
  | SpikesBlock Text
  | MedallionBlock Text
  | GrenadeBlock Text
  deriving (Eq, Generic, Show)

deriveBoth defaultOptions ''DescriptionBlock

describeAbilities ∷ Name → Gender → Abilities → [DescriptionBlock]
describeAbilities name gender (Abilities inborn obtained) =
  evaluatingState (seed name gender) do
    inbornDesc   ← inbornBreakdown name gender inborn
    obtainedDesc ← obtainedBreakdown name gender obtained
    pure (inbornDesc ++ obtainedDesc)

seed ∷ Name → Gender → StdGen
seed (Name name) gender = mkStdGen (nameAsInt + genderAsInt) where
  nameAsInt   = Unsafe.read . concatMap (show . ord) $ toString name
  genderAsInt = fromEnum gender

inbornBreakdown ∷ Name → Gender → Maybe Metalborn → Rand [DescriptionBlock]
inbornBreakdown (Name (txt → name)) gender mb =
  let single a = singleton . a . applyGender gender
      allo = single AllomancyBlock
      feru = single FeruchemyBlock
      mistborn fe = mistbornDesc name fe <&> (\(a, f) → allo a ++ maybe [] feru f)
      feruc fe = feruchemistDesc name fe <&> (\(a, f) → feru a ++ maybe [] allo f)
   in case mb of
    Nothing                                    → allo <$> noAbilityDesc name
    Just (Singleborn (Misting misting))        → allo <$> mistingDesc name misting
    Just (Singleborn (Ferring ferring))        → feru <$> ferringDesc name ferring
    Just (Halfborn (Mistborn maybeFerring))    → mistborn maybeFerring
    Just (Halfborn (Feruchemist maybeMisting)) → feruc maybeMisting
    Just Fullborn                              → allo <$> fullbornDesc name
    Just (Twinborn mist ferr twin')            → twinbornDesc name gender mist ferr twin'

obtainedBreakdown ∷ Name → Gender → AbilitiesObtained → Rand [DescriptionBlock]
obtainedBreakdown (Name (txt → name)) gender (AbilitiesObtained spkA spkF mdlF grn) = do
  spkReason ← randEl
                [ name ⊕ ", after defeating Hemalurgists, came into possession of"
                , name ⊕ " was entrusted by Harmony with"
                , name ⊕ " acquired in mysterious circumstances"
                ]
  mdlReason ← randEl
                [ name ⊕ ", after a battle with Southern Scadrian criminals, acquired"
                , name ⊕ " was entrusted by the Sovereign with"
                , name ⊕ " acquired in mysterious circumstances"
                ]
  grnExpl   ← randEl
                [ name ⊕ " owns a South Scadrian Allomantic Grenade, which can be charged with Allomantic or Feruchemical power and thrown for remote activation."
                ]
  let expl ∷ GenderNeutral → GenderNeutral → Int → [Metal] → [Metal] → GenderNeutral
      expl thing reason num a f =
        reason ⊕ " **" ⊕ show num ⊕ " " ⊕ thing ⊕ (if num > 1 then "s" else "") ⊕ "**, "
        ⊕ (case (null a, null f) of
             (False, False) → access ⊕ "**Allomantic " ⊕ oxford (map show a)
                            ⊕ "**, as well as **Feruchemical " ⊕ oxford (map show f) ⊕ "**."
             (False, True)  → access ⊕ "**Allomantic " ⊕ oxford (map show a) ⊕ "**."
             (True, False)  → access ⊕ "**Feruchemical " ⊕ oxford (map show f) ⊕ "**."
             (True, True)   → ""
          )
      access  = "granting " ⊕ them ⊕ " access to, or a boost of power in, "
      spkNum  = length (spkA ++ spkF)
      spkExpl = expl "Hemalurgic spike" spkReason spkNum spkA spkF
      mdlNum  = length mdlF
      mdlExpl = expl "Southern Medallion" mdlReason mdlNum [] mdlF

  pure $ [SpikesBlock    (applyGender gender spkExpl) | spkNum ≢ 0]
       ⊕ [MedallionBlock (applyGender gender mdlExpl) | mdlNum ≢ 0]
       ⊕ [GrenadeBlock   (applyGender gender grnExpl) | grn]

mistingDesc ∷ GenderNeutral → Misting → Rand GenderNeutral
mistingDesc name misting = do
  basic ← basicDesc name (ppGn misting) (ppGn $ mistingMetal misting) "Allomantic"
  pure $ basic ⊕ " " ⊕ txt (mistingDetails misting)

ferringDesc ∷ GenderNeutral → Ferring → Rand GenderNeutral
ferringDesc name ferring = do
  basic ← basicDesc name (ppGn ferring) (ppGn $ ferringMetal ferring) "Feruchemical"
  pure $ basic ⊕ " " ⊕ txt (ferringDetails ferring)

twinbornDesc ∷ GenderNeutral → Gender → Misting → Ferring → Maybe Twinborn → Rand [DescriptionBlock]
twinbornDesc name gender mist ferr twin = do
  let single a = a . applyGender gender
  connector ← randEl ["Furthermore", "Moreover", "Additionally", "In addition"]
  mDesc ← single AllomancyBlock <$> mistingDesc name mist
  fDesc ← single FeruchemyBlock
            <$> ((\t → connector ⊕ ", " ⊕ t) . uncapitalize <$> ferringDesc name ferr)
  pure
    [ mDesc
    , fDesc
    , single TwinbornBlock $
        ( case twin of
          Nothing →
           name ⊕ "'s Twinborn combination is rare, and there is no common name for it."
          Just Crasher →
           "As a Coinshot and a Skimmer, " ⊕ name ⊕ " is said to be a **Crasher**. " ⊕ they ⊕ " have the same powerful abilities as Waxillium Ladrian."
          Just twinName →
            "Some call the combination of a " ⊕ ppGn mist ⊕ " and " ⊕ ppGn ferr ⊕ " a **" ⊕ ppGn twinName ⊕ "**."
        )
      ⊕ (if mistingMetal mist ≡ ferringMetal ferr
          then " " ⊕ compounderAnecdote name (mistingMetal mist)
          else ""
        )
      ⊕ (case comboAnecdote mist ferr of
          Nothing       → ""
          Just anecdote → " " ⊕ anecdote
        )
      ⊕  " Twinborn combinations create subtle new effects which are not well known at this time."
    ]

mistbornDesc ∷ GenderNeutral → Maybe Ferring → Rand (GenderNeutral, Maybe GenderNeutral)
mistbornDesc name ferr = do
  ferrInfo ← case ferr of
     Nothing → pure Nothing
     Just f → do
       ferrDesc ← ferringDesc name f
       pure . Just $ "Additionally, " ⊕ name ⊕ " is a Ferring. " ⊕ ferrDesc
                   ⊕ "\n\n" ⊕ compounderAnecdote name (ferringMetal f)
  reasonForBeingMistborn ← randEl
    [ " How " ⊕ name ⊕ " was able to obtain this power of legends is a story one can only hear from " ⊕ themselves ⊕ "."
    , " " ⊕ name ⊕ " has become a Mistborn through unexpected circumstances on a mission to protect Scadrial's Perpendicularity from offworld invaders."
    , " " ⊕ name ⊕ " was bestowed one of the last remaining beads of Lerasium by Harmony's agents to fight a deadly threat from offworld."
    , " " ⊕ name ⊕ " is the first known natural Mistborn in centuries, a descendent of the Lord Mistborn himself."
    ]
  pure
    ( "**" ⊕ name ⊕ "** is a **Mistborn** able to burn all Allomantic metals." ⊕ reasonForBeingMistborn
    , ferrInfo
    )

feruchemistDesc ∷ GenderNeutral → Maybe Misting → Rand (GenderNeutral, Maybe GenderNeutral)
feruchemistDesc name mist = do
  mistInfo ← case mist of
    Nothing → pure Nothing
    Just m → do
     mistDesc ← mistingDesc name m
     pure . Just $ "Additionally, " ⊕ name ⊕ " is a Misting. "<> mistDesc
                 ⊕ "\n\n" ⊕ compounderAnecdote name (mistingMetal m)
  reasonForBeingFeruchemist ← randEl
    [ " How " ⊕ name ⊕ " was able to obtain this power of legends is a story one can only hear from " ⊕ themselves ⊕ "."
    , " " ⊕ name ⊕ " has become a Feruchemist through unexpected circumstances on a mission to protect Scadrial's Perpendicularity from offworld invaders."
    , " Harmony was able to exceptionally bestow Feruchemical abilities to " ⊕ name ⊕ " for a critical offworld mission, thanks to " ⊕ name ⊕ "'s unique religious fervor."
    , " " ⊕ name ⊕ " is the first known natural Feruchemist in centuries, descendent of a reclusive Terris community in the Northern Roughs."
    ]
  pure
    ( "**" ⊕ name ⊕ "** is a **Feruchemist**. " ⊕ they ⊕ " can store all Feruchemical attributes." ⊕ reasonForBeingFeruchemist
    , mistInfo
    )

fullbornDesc ∷ GenderNeutral → Rand GenderNeutral
fullbornDesc name =
  pure $ "**" ⊕ name ⊕ "** is both a Mistborn and a Feruchemist, able to burn all Allomantic metals and store all Feruchemical attributes. How " ⊕ name ⊕ " was able to obtain these powers of legends, held by only the Lord Ruler before, is a story one can only hear from " ⊕ themselves ⊕ ". Those who know of " ⊕ their ⊕ " abilities consider " ⊕ them ⊕ " one of the most powerful humans in the Cosmere."

compounderAnecdote ∷ GenderNeutral → Metal → GenderNeutral
compounderAnecdote name metal =
  name ⊕ " can access unlimited Feruchemical " ⊕ ppGn (feruchemicalAttribute metal) ⊕ " via **Compounding**."

basicDesc ∷ GenderNeutral → GenderNeutral → GenderNeutral → GenderNeutral → Rand GenderNeutral
basicDesc name singlebornKind metal metallicKind = randEl
  [ "**" ⊕ name ⊕ "** is a" ⊕ maybeN ⊕ " **" ⊕ singlebornKind ⊕ "**: " ⊕ they ⊕ " can use **" ⊕ metallicKind ⊕ " " ⊕ metal ⊕ "**."
  , "As a" ⊕ maybeN ⊕ " **" ⊕  singlebornKind ⊕ "**, **" ⊕ name ⊕ "** can use **" ⊕ metallicKind ⊕ " " ⊕ metal ⊕ "**."
  , "**" ⊕ name ⊕ "** can use **" ⊕ metallicKind ⊕ " " ⊕ metal ⊕ "**, making " ⊕ them ⊕ " a" ⊕ maybeN ⊕ " **" ⊕ singlebornKind ⊕ "**."
  ]
  where
  maybeN = if (∈ "aeioAEIO") . Unsafe.head . toString $ applyGender Male singlebornKind
              then "n"
              else ""

noAbilityDesc ∷ GenderNeutral → Rand GenderNeutral
noAbilityDesc name = randEl
  [ name ⊕ " has no inborn ability to use the Metallic Arts."
  , name ⊕ " is, " ⊕ " unfortunately, not the Hero of Ages. The Hero of Ages was a powerful Metalborn, which " ⊕ name ⊕ " isn't. In fact, " ⊕ they ⊕ " " ⊕ were ⊕ "born with no Allomantic or Feruchemical powers whatsoever."
  ]

mistingDetails ∷ Misting → Text
mistingDetails = \case
  Coinshot →
    "Coinshots use their Steel sight to detect and Push metals away in a straight line. They can use this ability to fly through the air by Pushing metals placed on the ground. Coinshots are among the deadliest Metalborn, capable of enhancing the power of gunfire or use small metallic objects (such as coins) as weapons."
  Lurcher →
    "Lurchers use their Iron sight to detect and Pull metals towards themselves in a straight line. By Pulling on a metal heavier than themselves (or anchored to something that is), a Lurcher can fly in a limited fashion. Lurchers' powers can be used defensively in a party, by Ironpulling metal projectiles into a shield."
  Rioter →
    "Rioters can enflame other people's emotions in a controllable area of effect. The Rioter can choose what emotion to Riot. Certain rioters known as zinc counselors use their abilities as a kind of therapy."
  Soother →
    "Soothers can decrease other people's emotions in a controllable area of effect. The Soother can choose what emotion to Soothe. There is evidence of consensual emotional counselling."
  Thug →
    "Thugs can increase their strength, ability to heal, balance, reaction speed and resistance to extreme temperatures. Pewter roughly doubles the average person's strength, and triples it when flared."
  Tineye →
    "Tineyes can increase the sensitivity of their senses. Flaring Tin causes a huge burst of sensory input clearing the Tineye's pain or exhaustion."
  Smoker →
    "Smokers can protect nearby Allomancers from being detected by Seekers (Bronze Allomancers). In addition, a Smoker is immune to emotional Allomancy while burning Copper. It is said to be possible for Smokers to shield others from emotional Allomancy, but the requirements to achieve this feat are not well known."
  Seeker →
    "Seekers are able to detect nearby use of Allomancy."
  DuraluminGnat →
    "Duralumin Gnats can, while burning Duralumin, expend their entire reserve of Duralumin as well as any other metal they are burning at the same time. Unfortunately, most Duralumin Gnats cannot utilize their power, as the ability to burn a metal other than Duralumin itself can only be acquired via difficult means such as Hemalurgy or rare unsealed metalminds."
  AluminumGnat →
    "Aluminum Gnats can empty their reserve of burnable Allomantic Metals by burning Aluminum. This is already of little use, but to make matters worse, the ability to burn a metal other than Aluminum itself can only be acquired via difficult means such as Hemalurgy or rare unsealed metalminds."
  Augur →
    "Augurs can see visions of Gold Shadows: alternate selves that the Augur could have been."
  Oracle →
    "Oracles can see visions of their own possible near-future. Historically, Electrum has been used in the events preceding the Catacendre as a counter to Atium. There is currently unexplored potential in the combination of Allomantic Electrum and Nicrosil (or Duralumin)."
  Nicroburst →
    "A Nicrobust burning Nicrosil while touching another Allomancer will cause the latter's reserves of currently-burning metals to be expended all at once in a huge burst of power. This can be used on both allies and foes depending on the situation."
  Leecher →
    "Any Allomancers touched by a Leecher burning Chromium will immediately lose their metal reserves."
  Pulser →
    "Pulsers can create Cadmium bubbles where time flows slower than outside the bubble. This can be useful to travel forward in time or temporarily incapacitate anyone inside the speed bubble from an outsider's perspective. Bullets crossing the bubble's border seem to change course in unpredictable ways."
  Slider →
    "Sliders can create Bendalloy bubbles where time flows faster than outside the bubble. This can be useful to move faster or isolate people inside the bubble, for example to have a conversation with allies or engage an enemy one-on-one. Bullets crossing the bubble's border seem to change course in unpredictable ways."

ferringDetails ∷ Ferring → Text
ferringDetails = \case
  Skimmer →
    "Skimmers can store body weight, freely lowering their weight to safely fall any distance or tapping weight to become heavier. This can be used for combat by adding force to the Skimmer's blows or using weight to withstand attacks. Tapping weight can be used to cause a Coinshot attempting to Push a Skimmer to be blown backwards."
  Steelrunner →
    "Steelrunners can store physical speed. By tapping their steelmind, Steelrunners can move many times faster than a normal human, as well as burn metals faster. Steelrunners cannot ignore wind resistance and friction and will burn up if they run too quickly."
  Sparker →
    "Sparkers can store mental speed. By tapping their zincminds, Sparkers can think very quickly and come to conclusions faster."
  Firesoul →
    "Firesouls can store warmth. While filling a brassmind, a Firesoul will be cool. This can be used to resist extreme temperatures, both hot and cold."
  Windwhisperer →
    "Windwhisperers can store sensitivity of senses. One sense may be stored each in a tinmind."
  Brute →
    "Brutes can store strength. Unlike Thugs (Pewter Allomancers), Brutes actually change their muscle mass and become physically larger."
  Archivist →
    "Archivists can store memories inside copperminds. When an Archivist stores a memory in a coppermind, it immediately disappears from their memory. Memories stored in a coppermind are filed individually and are not seen as one whole memory to the Archivist."
  Sentry →
    "Sentries can store wakefulness. This can be used to be inhumanly alert and energetic, or conversely to cure one's insomnia."
  Spinner →
    "A Spinner can store Fortune. The precise mechanics of Fortune are unknown at this time."
  Soulbearer →
    "Soulbearers can store Investiture. The precise mechanics of Feruchemical Investiture are unknown at this time."
  Gasper →
    "Gaspers can store breath. While storing breath, the Gasper must hyperventilate to keep their body oxygenated. Tapping a cadmiummind allows the user to go without breathing, or to introduce extra oxygen into their bloodstream."
  Subsumer →
    "Subsumers can store nutrition and calories. While filling a bendalloymind, a Subsumer is able to eat large quantities of food without becoming full. Tapping such a metalmind will allow the user to go without food. A separate bendalloymind can be used to store fluid intake."
  Trueself →
    "Trueselves can store Identity. The precise mechanics of Identity are unknown at this time."
  Connector →
    "Connectors can store Connection. Filling a duraluminmind can be used to reduce other people's awareness of and friendship with the Connector, as these Spiritual Connections become stored away. Tapping it would strengthen Connections or allow the Connector to form relationships faster. Southern Scadrians use unsealed duraluminminds to enable them to communicate with others on foreign lands."
  Bloodmaker →
    "Bloodmakers can store health, becoming frail while storing and able to heal from fatal injuries while tapping it."
  Pinnacle →
    "Pinnacles can store determination. A Pinnacle filling an electrummind will become depressed, and can tap it later to enter a manic state."

comboAnecdote ∷ Misting → Ferring → Maybe GenderNeutral
comboAnecdote mist ferr = case (mist, ferr) of
  -- Mistborn Adventure Game combos
  -- (I changed up some of them if I didn't understand them or if they seemed to
  -- assume non-canon properties of Identity, Fortune, etc...)
  (Tineye, Windwhisperer)    → Just "Eagle Eyes are known to be frighteningly perceptive."
  (Tineye, Steelrunner)      → Just "No one can escape a Catcher's sight or speed."
  (Tineye, Archivist)        → Just "Monitors are detail-oriented and meticulous."
  (Tineye, Sparker)          → Just "Quickwits can adjust plans at lightning speed."
  (Tineye, Spinner)          → Just "Keeneyes cannot be cheated out of a victory at cards."
  (Thug, Brute)              → Just "Hefters live for physical challenges."
  (Thug, Steelrunner)        → Just "Sprinters are extremely difficult to best in a fistfight."
  (Thug, Sparker)            → Just "Sooners are fearsome fighters exploiting any opportunity."
  (Thug, Spinner)            → Just "Scrappers are known to have the best drunken brawl stories."
  (Thug, Bloodmaker)         → Just "Brutebloods are never counted out of a fight."
  (Thug, Gasper)             → Just "Marathoners are known for being utterly tireless."
  (Lurcher, Brute)           → Just "Scalers scale buildings with great speed and grace."
  (Lurcher, Skimmer)         → Just "Deaders are notorious for smashing themselves flat."
  (Lurcher, Steelrunner)     → Just "Guardians are tremendously popular bodyguards."
  (Lurcher, Sparker)         → Just "Navigators are nimble and quick-witted Ironswingers."
  (Coinshot, Windwhisperer)  → Just "Sharpshooters are crack shots and deadly gunfighters."
  (Coinshot, Skimmer)        → Just "Crashers are exceptionally destructive and dangerous."
  (Coinshot, Steelrunner)    → Just "Swifts are fast and destructive, often criminals."
  (Coinshot, Trueself)       → Just "Shrouds are known to be nameless assassins."
  (Coinshot, Connector)      → Just "Bigshots are often leaders of law enforcement and gangs."
  (Coinshot, Spinner)        → Just "Luckshots frequently evade death and have a deadly reputation."
  (Coinshot, Gasper)         → Just "Cloudtouchers soar to heights that no others can reach."
  (Smoker, Archivist)        → Just "Copperkeeps have inhumanly good memory."
  (Smoker, Sentry)           → Just "Shrouds are great allies to criminal crews."
  (Smoker, Firesoul)         → Just "Boilers are tough-to-track-down survivalists"
  (Smoker, Trueself)         → Just "Ghostwalkers seem benenath everyone's notice."
  (Smoker, Connector)        → Just "Shelters are Allomancers' best friends."
  (Smoker, Spinner)          → Just "Maskers are a blessing to any Allomancer crew."
  (Seeker, Windwhisperer)    → Just "Sentinels are masters of all six senses."
  (Seeker, Steelrunner)      → Just "Hazedodgers are skilled Allomancer hunters."
  (Seeker, Archivist)        → Just "Metalmappers are meticulous chroniclers of Metalborn."
  (Seeker, Sentry)           → Just "Sleepless are overstimulated and overly sensitive."
  (Seeker, Sparker)          → Just "Mistings find Pulsewises impossible to trick."
  (Seeker, Subsumer)         → Just "Stalkers pursue their quarry without end."
  (Rioter, Brute)            → Just "Strongarms wins opponents over, one way or the other."
  (Rioter, Sparker)          → Just "Masterminds can outthink or mess with anyone's head."
  (Rioter, Trueself)         → Just "Loudmouths are strongly opinionated."
  (Rioter, Connector)        → Just "Zealots are frighteningly persuasive."
  (Rioter, Spinner)          → Just "Highrollers turn everything into a high-stakes gamble."
  (Soother, Sparker)         → Just "Schemers are quick-witted deceivers."
  (Soother, Firesoul)        → Just "Coolers chills bodies as well as hot tempers."
  (Soother, Trueself)        → Just "Icons are natural leaders."
  (Soother, Connector)       → Just "Pacifiers are excellent peacemakers."
  (Soother, Spinner)         → Just "Slicks are smooth talkers and lucky varmints."
  (Soother, Pinnacle)        → Just "Resolutes are masterful negotiators and diplomats."
  (AluminumGnat, Trueself)   → Just "Puremind are self-made, self-assured, and usually rich."
  (DuraluminGnat, Connector) → Just "Friendlies are blissed-out, likeable weirdos."
  (Leecher, Brute)           → Just "Metalbreakers never fight fair."
  (Leecher, Spinner)         → Just "Ringers are incredibly and unfairly lucky."
  (Leecher, Subsumer)        → Just "Gulpers consume physical and magical energy."
  (Nicroburst, Brute)        → Just "Boosters provide physical and mystical support."
  (Nicroburst, Archivist)    → Just "Burst Tickers never forget a favor done for a Misting."
  (Nicroburst, Connector)    → Just "Enablers feed both magic and ego."
  (Nicroburst, Soulbearer)   → Just "Soulbursts may be of great importance to the Cosmere."
  (Nicroburst, Pinnacle)     → Just "Cohorts are the fearless sidekicks of string Allomancers."
  (Augur, Archivist)         → Just "Chroniclers record the past so that they do not repeat it."
  (Augur, Trueself)          → Just "Vessels are known to change personality overnight."
  (Augur, Bloodmaker)        → Just "Timeless are unkillable and rumored to be immortal."
  (Augur, Pinnacle)          → Just "Introspects analyze every detail of their lives."
  (Oracle, Skimmer)          → Just "Whimflitters are prone to change plans."
  (Oracle, Sparker)          → Just "Flickers have the fastest reaction times in the world."
  (Oracle, Spinner)          → Just "Charmed are impossible to ambush."
  (Oracle, Pinnacle)         → Just "Visionaries see the future and face it boldly."
  (Pulser, Sentry)           → Just "Plotters are skilled at executing long-term plans."
  (Pulser, Bloodmaker)       → Just "Yearspanners appear to have extraordinarily long lifespans."
  (Pulser, Gasper)           → Just "Chrysalises endure dire situations until things improve."
  (Slider, Windwhisperer)    → Just "Spotters are able to take in every detail."
  (Slider, Steelrunner)      → Just "Blurs are inhumanly productive."
  (Slider, Archivist)        → Just "Assessors can break down events second by second."
  (Slider, Sparker)          → Just "Flashwits are expert negotiators and planners."
  (Slider, Trueself)         → Just "Monuments can flip from dull to magnetic instantly."
  (Slider, Bloodmaker)       → Just "Constants seem unaffected by time or age."
  (Slider, Pinnacle)         → Just "Transcendents face the unknown without hesitation."
  (Slider, Subsumer)         → Just "Sated are ascetic loners."
  _                          → Nothing

ppGn ∷ Show a ⇒ a → GenderNeutral
ppGn = txt . toText . pp

oxford ∷ (IsString s, Monoid s) ⇒ [s] → s
oxford []     = ""
oxford [x]    = x
oxford (x:xs) = x ⊕ (if length xs ≡ 1 then " and " else ", ") ⊕ oxford xs
