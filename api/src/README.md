# `/api/src`

Most modules in this project are namespaced to `App.*`. This way:

- You can distinguish domain-relevant stuff from what's in `/api/src` which is
  more peripheral utilities like `Prelude.hs`, `Config.hs`, and `REPL.hs`--
  the kind of stuff that would live in `/api/` if it weren't a Haskell source
  file.
- Import lists cleanly separate project modules from library imports.
