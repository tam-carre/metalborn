# Metalborn frontend

## Running

You must first run the API server.  Refer to [`/api/README.md`](../api/) for instructions. Running the API server will additionally generate `./src/API.elm` which is not version-controlled.

Then run `npm i && npm run dev` in the `/webapp` directory (this one).

## Coding conventions

- Formatted with elm-format
- [`Some encapsulation best practices applied in this project`](./docs/Elm_encapsulation_best_practices.md)
- [`Retrospective on attempt to (conservatively) use lenses in this project`](./docs/Elm_When_to_use_lenses_How_to_generate_them.md)
