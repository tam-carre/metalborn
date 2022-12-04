# Elm: When to use lenses; How to generate them

## When to use lenses

Elm code should not be clever, and should be simple. Lenses used conservatively can actually further those goals. In practice, that means mostly:

### Nested record updates where a nested model of the domain is the correct choice

There are times when accessing nested data is a sign of poor
[encapsulation](./docs/Elm_encapsulation_best_practices.md). However, sometimes
it is the best representation of the domain, and Elm syntax punishes you heavily
for it:

```elm
setAddressNumberTo1 person =
    { person
        | address =
            person.address
                |> (\address -> { address | number = 1 })
    }

-- or maybe
setAddressNumberTo1 person =
    let
      address = person.address
    in
    { person | address = { address | number = 1 }) }
```

Compare with using lenses:

```elm
setAddressNumberTo1 person =
    person |> set (F.address << F.number) 1

-- can also be eta-reduced if you'd like
setAddressNumberTo1 =
    set (F.address << F.number) 1
```

The syntax is not even complicated! Lens difficulty is highly overstated. Their implementation is difficult, but their basic API is very simple.

### Applying a function over record fields

The syntax for applying a function over record fields is very expensive in Elm, even when the data is not nested:

```elm
capitalizeLastName person =
    { person | lastName = person.lastName |> String.toUpper }

-- nested data
capitalizeStreet person =
    { person
        | address =
            person.address
                |> (\address -> { address | street = address.street |> String.toUpper })
    }

-- or maybe
capitalizeStreet person =
    let
      address = person.address
    in
    { person | address = { address | street = address.street |> String.toUpper }) }
```

Compare with using lenses:

```elm
capitalizeLastName person =
    person |> over F.lastName String.toUpper

-- again, you can optionally eta-reduce
capitalizeLastName =
    over F.lastName String.toUpper

-- nested data
capitalizeStreet =
    over (F.address << F.street) String.toUpper
```

Again, the syntax is simple; in fact much simpler than the language record syntax.

## How to generate lenses

This project uses `erlandsona/elm-accessors` V4.

`elm-review` has been configured such that you can generate into `src/Fields.elm` any new lens you use. Simply:

- Use the lens(es) you want to use, but make sure your module imports `Fields`

```elm
import Fields as F

capitalizeStreetName =
    over (F.address << F.street << F.name) String.toUpper
```

- Run `elm-review --fix-all` at the Elm project root
- The lenses `address`, `street` and `name` are now exported by `src/Fields.elm`

If you wish to learn more about how the `elm-review` rule works, check out
[elm-review-missing-record-field-lens](https://package.elm-lang.org/packages/lue-bird/elm-review-missing-record-field-lens/2.1.0/)
