# [Metalborn.io](https://metalborn.io/)

(TODO: Include demo video/gif here once app is more complete)

Welcome, traveller! And thanks for checking out my pet project, whether you came
from **[Metalborn.io](https://metalborn.io/)** itself or from my GitHub profile.

**Metalborn.io** is a choose-your-own-adventure-style character generator set in
the **Mistborn Era 2** universe.

What the choose-your-own-adventure part means really is that instead of a traditional navbar, navigation between pages is done via first-person user actions, and pages contain smart narration that differ depending on the user's past actions.

Of course this is all for flavour and just some playful frills to go alongside
generator's main purpose. "Storytelling through webdesign" seems to be a theme in my
projects. I like to transport my visitors to imaginary worlds:

- [My Yokohama Kaidashi Kikou fansite](https://alpha.cafe) is replicates the manga's feelings of nostalgia and of finding beautiful unvisited places in the countryside, with a quaint retro design that makes visitors feel like they've just stumbled upon one of the last fansites of its kind.
- [owmal](https://owmal.alpha.cafe) translates text into dozens of languages and feels like opening a mysterious, magical knowledge vault.

## Technical information

The project's frontend is written in Elm, and the API in Haskell.

Character generation is fully handled by the API server.

Documentation exists for both the Elm and Haskell codebases in [/webapp/](./webapp/) and [/api/](./api/) including how to run locally, techniques and conventions used, endpoint breakdown etc.

There is a [CI pipeline](https://github.com/tam-carre/metalborn/actions/runs/3633421876/jobs/6130418618) that runs Haskell and Elm builds, tests, formatting and linting checks.
