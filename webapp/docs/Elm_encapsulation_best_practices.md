# Elm encapsulation best practices

Encapsulation in Elm is a topic that I haven't come across much, and I ended up learning through experience what I personally find effective. Here are the practices I apply on this project and writing Elm in general.

## Goals

- Preventing module users from accidentally messing with low-level internals that exposes them to breaking changes and should be manipulated by the module itself
    - This helps contain complexity across the app as each module only deals with its own subdomain, while using high-level APIs from other subdomains.
- Making a module file reader able to easily distinguish the module's public API from its internals
    - It should be intuitive to inspect the module's high-level logic first, and then optionally descend into its internals
    - Using the export list for this carries too much cognitive overhead

The last point is more of a stylistic concern, but one I seem to oddly care about more than others as I find that it significantly affects my cognitive load while perusing code.

## Guidelines

### Main

**It can be evidently assumed that anything other than `main` is internal to `Main.elm`**, so no special measures are needed.

### Page modules

#### Predetermined set of exports

This is what a page's exports should look like:

```elm
module Page.Home exposing (Model {- OPAQUE -}, Msg {- OPAQUE -}, page)
```

`page` is a value like this:

```elm
-- src/Page/Character.elm
page : Page Model Msg (Maybe ( API.Name, API.Gender ))
page =
    { title = title
    , init = init
    , update = update
    , view = view
    }
```

The `Page` type formalizes what a page module should export, defined like this:

```elm
type alias Page model msg deps =
    { title : model -> String
    , init : deps -> ( model, Cmd msg )
    , update : Ctx -> msg -> model -> ( model, Cmd msg )
    , view : Ctx -> model -> Element msg
    }
```

**If any other utility function is exported from a page module it is a sign that it should live inside some other helper module.**

**Hence, any binding inside a page module other than `page` can be safely inferred by the reader to be internal!**

#### Models and Msg should be opaque

For models this means something like this:

```elm
type Ctx
    = Ctx Internal


type alias Internal =
    { device : Device
    , nkey : Nav.Key
    , previousRoute : Maybe Route
    , route : Maybe Route
    }

```

```elm
type Model
    = Model Internal


type alias Internal =
    { requestName : Maybe API.Name }
```

Doing this protects module users from exposing themselves to breaking changes by messing with record fields that are meant to be internal. It also ensures complex low-level operations are contained to the current module, controlling complexity projectwide.

What's more, it contributes to distinguishing internal helpers from public APIs: **if a function has `Internal` in its signature, it means the function is internal!**

For sum types (most Msgs) we don't do anything like that but we still use `OPAQUE` export tags, see next section.

### Explicit `OPAQUE` tags in opaque type exports

```elm
module Page.Home exposing (Model {- OPAQUE -}, Msg {- OPAQUE -}, page)
```

This highlights that the type constructors are not just unexported because they
happen to be currently unneeded outside this module: they are meant to never be
exported, now or in the future.

## Helper modules' low-level APIs hidden in `internal` records or `My.Module.Internal` submodules

Helper modules expose helper functions, as opposed to a simple `Page`. We therefore have to be use special conventions to make it easy for readers to distinguish public APIs from module internals.

This is what it looks like:

```elm
{-| Ensure low-level APIs remain private
-}
internal :
    { mapSizePalette : (a -> b) -> SizePalette a -> SizePalette b
    , mapColorPalette : (a -> b) -> ColorPalette a -> ColorPalette b
    }
internal =
    { mapSizePalette =
        \f palette ->
            { xs = f palette.xs
            , s = f palette.s
            , m = f palette.m
            , l = f palette.l
            }
    , mapColorPalette =
        \f palette ->
            { accent = f palette.accent
            , transparent = f palette.transparent
            , bg = f palette.bg
            , fg = f palette.fg
            , muted = f palette.muted
            , veryMuted = f palette.veryMuted
            }
    }
```

Or using a submodule; encouraged if there are many internal bindings or if you still want users to be able to import internals if they really want to (this might be useful for e.g. libraries where users have no way to add code to the library's modules without forking it)

```elm
-- src/Palette/Internal.elm

mapSizePalette : (a -> b) -> SizePalette a -> SizePalette b
mapSizePalette f palette =
    { xs = f palette.xs
    , s = f palette.s
    , m = f palette.m
    , l = f palette.l
    }


mapColorPalette : (a -> b) -> ColorPalette a -> ColorPalette b
mapColorPalette f palette =
    { accent = f palette.accent
    , transparent = f palette.transparent
    , bg = f palette.bg
    , fg = f palette.fg
    , muted = f palette.muted
    , veryMuted = f palette.veryMuted
    }
```

```elm
-- src/Palette.elm

import Palette.Internal (mapSizePalette, mapColorPalette)
```

Another viable module naming scheme is `Internal.My.Module`.

Since the first option has a little readability overhead, and the second option introduces some project structural complexity, which option to use or whether to use one at all is on a case-by-case basis.
