# Metalborn frontend

## Running

Currently, the Metalborn frontend can be tested by running `npm i && npx elm reactor` in the `webapp` directory and visiting `http://localhost:8000/src/Main.elm`

You'll also need `./src/API.elm` which is code-generated and not version controlled. For that, you simply need to run the API server. Refer to [`/api/README.md`](../api/) for instructions.

## Coding conventions

- Formatted with elm-format
- [`Some encapsulation best practices applied in this project`](./docs/Elm_encapsulation_best_practices.md)
- [`Retrospective on attempt to (conservatively) use lenses in this project`](./docs/Elm_When_to_use_lenses_How_to_generate_them.md)
