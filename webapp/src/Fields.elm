module Fields exposing (character, ctx, requestName)

{-| For information about usage and generation of lenses in this project,
please read `../docs/Elm_When_to_use_lenses_How_to_generate_them.md`
-}

import Accessors exposing (Lens, lens)


character : Lens ps { r | character : a } a x y
character =
    lens ".character" .character (\rec val -> { rec | character = val })


ctx : Lens ls { r | ctx : a } a x y
ctx =
    lens ".ctx" .ctx (\rec val -> { rec | ctx = val })


requestName : Lens ls { r | requestName : a } a x y
requestName =
    lens ".requestName" .requestName (\rec val -> { rec | requestName = val })
