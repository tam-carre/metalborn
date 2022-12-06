# Metalborn API

## Running

### Database

You can run a local database if you have docker and docker compose V2 installed
simply by running `docker compose up --build`.  If you want to connect to a different database,
you can change the database connection data in [`src/Config.hs`](./src/Config.hs).

### Build

Install [GHCup](https://www.haskell.org/ghcup/) and use `ghcup tui` to make sure
GHC 9.2.4 and Cabal are installed. Then optionally install `hpack`.

```sh
cabal update
cabal install hpack
```

You'll need to run `hpack` when you edit `package.yaml`, rename a module, or create a new one. It will generate a new `metalborn-api.cabal` file -- the latter is still version controlled so that CI pipelines do not need to install `hpack` which can take a while.

Then run:

`cabal run metalborn-api-exe`

You should see this output:

```
Endpoint documentation generated.
Elm API types and functions generated.
Starting Metalborn API server.
```

GHC might emit error messages due to missing libraries. On Ubuntu, the following
will cover you:

```
sudo apt update
sudo apt install -y build-essential libgmp3-dev zlib1g-dev libpq-dev libtinfo-dev
```

On other systems, please look up how to install the libraries that GHC is asking
you for.

You can then check that the database is up and accessible by running:

```
curl -H 'Content-Type: application/json' localhost:8081/api/character/ -d '["Kaladin", "Male"]
```

Be aware that you can generate a searchable documentation of the module tree and
every module's exported functions' types and doc comments by running `cabal haddock`.

## Project map

### Coding conventions

#### Lenses

This repo uses `generic-lens` and `OverloadedRecordDot` on a per-file basis.
I wrote [a simple introduction to `generic-lens`](https://github.com/tam-carre/generic-lens-modern-setup) in order to make it very easy to pick up.

#### Use hlint to make custom combinators more discoverable

It's very easy to create new hlint rules for this sort of thing. Say you create
these functions in `App.Utils`:

```hs
posit ∷ MonadFail m ⇒ Bool → Text → m ()
posit cond e = if cond then pass else fail (toString e)

onFail ∷ l → Maybe r → Either l r
onFail = maybeToRight
```

just add this in `.hlint.yaml`:

```yaml
- warn:
    # other rules already ensure `pass` is used over `pure ()` and `pure` over
    # `return` so we don't need to write any rule using `return` or `pure ()`
    # note that placeholder variables must be only one character, e.g.
    # `if c`, not `if cond`
    lhs: "if c then pass else fail e"
    rhs: "App.Utils.posit c e"

- warn:
    lhs: "maybeToRight"
    rhs: "App.Utils.onFail"
```

### Formatted with `.stylish-haskell.yaml` and Unicode operators

Since this is my personal project, I have some fun with `UnicodeSyntax` and
`base-unicode-symbols`. Most of the symbols are entered with a hacky Vimscript
search-and-replace on filesave.

I use a [custom Prelude](./src/Prelude.hs) that re-exports `base-unicode-symbols`'s modules.

Here's what I put in `~/.vim/after/ftplugin/haskell.vim`:

<details>
  <summary>`~/.vim/after/ftplugin/haskell.vim`</summary>
  
  ```vim
  setlocal shiftwidth=2
  setlocal tabstop=2
  setlocal expandtab

  set formatprg=stylish-haskell

  function! Unicodize()
      let l = 1
      for line in getline(1,"$")
          call setline(
          \ l,
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(
          \ substitute(line,
          \ ' :: ', ' ∷ ', 'ge'),
          \ ' forall ',  ' ∀ ', 'ge' ),
          \ '->', '→', 'ge' ),
          \ '<-', '←', 'ge' ),
          \ '`notElem`', '∉', 'ge' ),
          \ '`elem`', '∈', 'ge' ),
          \ ' =<< ', ' =≪ ', 'ge' ),
          \ ' >>= ', ' ≫= ', 'ge' ),
          \ ' <=< ', ' ↢ ', 'ge' ),
          \ ' >=> ', ' ↣ ', 'ge' ),
          \ ' << ', ' ≪ ', 'ge' ),
          \ ' >> ', ' ≫ ', 'ge' ),
          \ ' >>> ', ' ⋙ ', 'ge' ),
          \ '-- ⋙ ', '-- >>> ', 'ge' ),
          \ ' && ', ' ∧ ', 'ge' ),
          \ ' || ', ' ∨ ', 'ge' ),
          \ ' ==', ' ≡', 'ge' ),
          \ '/=', '≢', 'ge' ),
          \ ' >= ', ' ≥ ', 'ge' ),
          \ ' <= ', ' ≤ ', 'ge' ),
          \ '>>^.', '≫^.', 'ge' ),
          \ ' <> ', ' ⊕ ', 'ge' )
          \)
          let l = l + 1
      endfor
  endfunction

  function! Frmt()
    if (&ft=='haskell')
      call Unicodize()
      call CocAction('format')
    endif

    let timer = timer_start(0, 'Cont', {})
    func! Cont(timer)
      if (&ft=='haskell')
        call Unicodize()
      endif
      update
    endfunc
  endfunction

  nnoremap <silent> <C-s> :call Frmt() <CR>
  vnoremap <silent> <C-s> <C-C>:call Frmt()<CR>
  inoremap <silent> <C-s> <C-O>:call Frmt()<CR><Esc>
  nnoremap <silent> <C-p> :call Frmt()<CR>
  vnoremap <silent> <C-p> <C-C>:call Frmt()<CR>
  inoremap <silent> <C-p> <C-O>:call Frmt()<CR><Esc>
  ```
  
</details>

### Tests

As far as testing effects go, the effectful modules in the app are:

- [`App.DB`](./src/App/DB.hs)
- [`App.Server`](./src/App/Server.hs)

That's not too much to worry about. [`App.Server`](./src/App/Server.hs) is a simple Servant server;
with Servant's strong type safety we're OK here. [`App.DB`](./src/App/DB.hs) is tested in [`DbSpec`](./test/DBSpec.hs) using an integration test running against a real database.

The main pure tests are in [`CharacterAbilitiesSpec`](./test/CharacterAbilitiesSpec.hs) which ensure the randomly generated abilities are within expectations given the default probability settings.

[`CharacterDescriptionSpec`](./test/CharacterDescriptionSpec.hs) saves descriptions to `./test/testDescriptions.txt` for manual editing, proofreading etc.


### Domain logic

The core of the domain logic lives in:
- The random abilities generator in [`App.Character.Abilities`](./src/App/Character/Abilities.hs)
- The random description generator in [`App.Character.Description`](./src/App/Character/Description.hs)

### Domain

The core of the domain is [`App.Character`](./src/App/Character.hs):

```hs
data Character
  = Character Name Gender Abilities [DescriptionBlock]
```

Which branches out into (most notably) [`App.Character.Abilities`](./src/App/Character/Abilities.hs), [`App.Character.Metalborn`](./src/App/Character/Metalborn.hs) and [`App.Character.Description`](./src/App/Character/Description.hs).

Here's `Abilities` and `Metalborn`:
```hs
data Abilities
  = Abilities (Maybe Metalborn) AbilitiesObtained

data AbilitiesObtained
  = AbilitiesObtained
    { spikedA ∷ [Metal]
    , spikedF ∷ [Metal]
    , medallF ∷ [Metal]
    , grenade ∷ Bool
    }

data Metalborn
  = Singleborn Singleborn
  | Twinborn Misting Ferring (Maybe Twinborn)
  | Halfborn Halfborn
  | Fullborn

data Singleborn
  = Misting Misting
  | Ferring Ferring

data Halfborn
  = Mistborn (Maybe Ferring)
  | Feruchemist (Maybe Misting)

data Misting = Coinshot | Lurcher | Rioter | Soother | Thug | Tineye | Smoker | Seeker | DuraluminGnat | AluminumGnat | Augur | Oracle | Nicroburst | Leecher | Pulser | Slider deriving

data Ferring = Skimmer | Steelrunner | Sparker | Firesoul | Windwhisperer | Brute | Archivist | Sentry | Spinner | Soulbearer | Gasper | Subsumer | Trueself | Connector | Bloodmaker | Pinnacle 

-- https://www.17thshard.com/forum/topic/97725-twinborn-names/
data Twinborn = EagleEye | Catcher | Monitor | Quickwit | Keeneye | Hefter | Sprinter | Sooner | Scrapper | Bruteblood | Marathoner | Scaler | Deader | Guardian | Navigator | Stalwart | Sharpshooter | Crasher | Swift | Shroud | Bigshot | Luckshot | Cloudtoucher | Copperkeep | Boiler | Ghostwalker | Shelter | Masker | Sentinel | Hazedodger | Metalmapper | Sleepless | Pulsewise | Stalker | Strongarm | Mastermind | Loudmouth | Zealot | Highroller | Instigator | Schemer | Cooler | Icon | Pacifier | Slick | Resolute | Puremind | Friendly | Metalbreaker | Ringer | Sapper | Gulper | Booster | BurstTicker | Enabler | Soulburst | Cohort | Chronicler | Vessel | Timeless | Introspect | Whimflitter | Foresight | Flicker | Charmed | Visionary | Plotter | Yearspanner | Chrysalis | Spotter | Blur | Assessor | Flashwit | Monument | Constant | Transcendent | Sated 

data Metal = Iron | Steel | Tin | Pewter | Zinc | Brass | Copper | Bronze | Cadmium | Bendalloy | Gold | Electrum | Chromium | Nicrosil | Aluminum | Duralumin
```
