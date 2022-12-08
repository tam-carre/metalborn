module Fields exposing (character, ctx, fullPower, grenade, medall, metalborn, probabs, requestName, spike, twinborn)

{-| For information about usage and generation of lenses in this project,
please read `../docs/Elm_When_to_use_lenses_How_to_generate_them.md`
-}

import Accessors exposing (Lens, lens)


character : Lens ls { r | character : a } a x y
character =
    lens ".character" .character (\rec val -> { rec | character = val })


ctx : Lens ls { r | ctx : a } a x y
ctx =
    lens ".ctx" .ctx (\rec val -> { rec | ctx = val })


medall : Lens ls { r | medall : a } a x y
medall =
    lens ".medall" .medall (\rec val -> { rec | medall = val })


grenade : Lens ls { r | grenade : a } a x y
grenade =
    lens ".grenade" .grenade (\rec val -> { rec | grenade = val })


spike : Lens ls { r | spike : a } a x y
spike =
    lens ".spike" .spike (\rec val -> { rec | spike = val })


fullPower : Lens ls { r | fullPower : a } a x y
fullPower =
    lens ".fullPower" .fullPower (\rec val -> { rec | fullPower = val })


twinborn : Lens ls { r | twinborn : a } a x y
twinborn =
    lens ".twinborn" .twinborn (\rec val -> { rec | twinborn = val })


metalborn : Lens ls { r | metalborn : a } a x y
metalborn =
    lens ".metalborn" .metalborn (\rec val -> { rec | metalborn = val })


probabs : Lens ls { r | probabs : a } a x y
probabs =
    lens ".probabs" .probabs (\rec val -> { rec | probabs = val })


requestName : Lens ls { r | requestName : a } a x y
requestName =
    lens ".requestName" .requestName (\rec val -> { rec | requestName = val })
