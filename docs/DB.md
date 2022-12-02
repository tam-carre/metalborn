# DB documentation

Metalborn data is stored using PostgreSQL.

For reference, or running your own database, see [`../api/docker/dbSchema.sql`](../api/docker/dbSchema.sql).
You can just run the docker container as explained in [`../api/README.md`](../api/README.md) though.

## Normalization & SQL constraints

Character data is fully normalized, which is no simple task if we wants to make
illegal states unrepresentable at the DB level.

The core component of the Character product type is this complex sum type:

```hs
data Metalborn
  = Singleborn Singleborn
  | Twinborn Misting Ferring (Maybe Twinborn)
  | Halfborn Halfborn
  | Fullborn

data Singleborn = Misting Misting | Ferring Ferring

data Halfborn = Mistborn (Maybe Ferring) | Feruchemist (Maybe Misting)

data Misting = Coinshot | Lurcher | Rioter | Soother | Thug | Tineye | Smoker | Seeker | DuraluminGnat | AluminumGnat | Augur | Oracle | Nicroburst | Leecher | Pulser | Slider

data Ferring = Skimmer | Steelrunner | Sparker | Firesoul | Windwhisperer | Brute | Archivist | Sentry | Spinner | Soulbearer | Gasper | Subsumer | Trueself | Connector | Bloodmaker | Pinnacle

data Twinborn = EagleEye | Catcher | Monitor | Quickwit | Keeneye | Hefter | Sprinter | Sooner | Scrapper | Bruteblood | Marathoner | Scaler | Deader | Guardian | Navigator | Stalwart | Sharpshooter | Crasher | Swift | Shroud | Bigshot | Luckshot | Cloudtoucher | Copperkeep | Boiler | Ghostwalker | Shelter | Masker | Sentinel | Hazedodger | Metalmapper | Sleepless | Pulsewise | Stalker | Strongarm | Mastermind | Loudmouth | Zealot | Highroller | Instigator | Schemer | Cooler | Icon | Pacifier | Slick | Resolute | Puremind | Friendly | Metalbreaker | Ringer | Sapper | Gulper | Booster | BurstTicker | Enabler | Soulburst | Cohort | Chronicler | Vessel | Timeless | Introspect | Whimflitter | Foresight | Flicker | Charmed | Visionary | Plotter | Yearspanner | Chrysalis | Spotter | Blur | Assessor | Flashwit | Monument | Constant | Transcendent | Sated
```

Phrasing it in human language, a Metalborn can either be *NULL*, or *Fullborn*, or *Halfborn (either Mistborn + an optional Ferring, or Feruchemist + an optional Misting)*, or *Twinborn (a mandatory Ferring, a mandatory Misting, an optional Twinborn[1])*, or *Singleborn (one of Misting or Ferring)*.

[1] Context: Twinborns have both Ferring and Misting powers. All 16 Ferring types and all 16 Misting types have names, but not all Twinborn combinations have names, there's 256 possible combinations.

Here is the basic representation in the DB:

| column name | SQL type |
| ------------- | ------------- |
| fullborn  | boolean  |
| halfborn  | enum{"Mistborn" \| "Feruchemist"} NULLABLE |
| twinborn  | enum{"Crasher" \| ...} NULLABLE |
| misting  | enum{"Coinshot" \| ...} NULLABLE |
| ferring  | enum{"Skimmer" \| ...} NULLABLE |

There's an obvious problem with that: tons of illegal states are representable.
For example, a row might say:

| column name | value |
| ------------- | ------------- |
| fullborn  | TRUE  |
| halfborn  | "Feruchemist" |
| twinborn  | NULL |
| misting  | NULL |
| ferring  | "Skimmer" |


How can a Fullborn also be a Feruchemist and a Ferring?

Or say:

| column name | value |
| ------------- | ------------- |
| fullborn  | FALSE  |
| halfborn  | NULL |
| twinborn  | "Crasher" |
| misting  | "Coinshot" |
| ferring  | NULL |

How can a a character be a Twinborn and have a Twinborn name, but not have
a Ferring name?

SQL constraints come to the rescue:

```SQL
ALTER TABLE characters ADD CONSTRAINT CK_NoInvalidMetalborn
CHECK (
  -- Fullborn
  ( fullborn
    AND halfborn IS NULL
    AND twinborn IS NULL
    AND misting IS NULL
    AND ferring IS NULL
  ) OR
  -- Halfborn (Mistborn (Maybe Ferring))
  ( NOT fullborn
    AND halfborn = 'Mistborn'
    AND twinborn IS NULL
    AND misting IS NULL
  ) OR
  -- Halfborn (Feruchemist (Maybe Misting))
  ( NOT fullborn
    AND halfborn = 'Feruchemist'
    AND twinborn IS NULL
    AND ferring IS NULL
  ) OR
  -- Twinborn Misting Ferring (Maybe Twinborn)
  ( NOT fullborn
    AND halfborn IS NULL
    AND ferring IS NOT NULL
    AND misting IS NOT NULL
  ) OR
  -- Singleborn Misting
  ( NOT fullborn
    AND halfborn IS NULL
    AND twinborn IS NULL
    AND ferring IS NULL
    AND misting IS NOT NULL
  ) OR
  -- Singleborn Ferring
  ( NOT fullborn
    AND halfborn IS NULL
    AND twinborn IS NULL
    AND ferring IS NOT NULL
    AND misting IS NULL
  )
)
```

Representable states are now equivalent to the Haskell encoding!
