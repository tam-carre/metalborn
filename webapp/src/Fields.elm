module Fields exposing (character, ctx, requestName)

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
