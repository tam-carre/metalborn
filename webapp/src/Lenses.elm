module Lenses exposing (character, requestGender, requestName)

import Accessors exposing (Lens, lens)


requestName : Lens ls { m | requestName : a } a x y
requestName =
    lens ".requestName" .requestName (\r v -> { r | requestName = v })


requestGender : Lens ls { m | requestGender : a } a x y
requestGender =
    lens ".requestGender" .requestGender (\r v -> { r | requestGender = v })


character : Lens ls { m | character : a } a x y
character =
    lens ".character" .character (\r v -> { r | character = v })
