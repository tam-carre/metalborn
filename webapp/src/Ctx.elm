module Ctx exposing (Ctx {- OPAQUE -}, init, phones, updateDevice)

import Element exposing (Device, DeviceClass(..), Orientation(..), classifyDevice)


type Ctx
    = Ctx Internal


type alias Internal =
    { device : Device }


init : Ctx
init =
    Ctx { device = { class = Desktop, orientation = Landscape } }


updateDevice : Int -> Int -> Ctx -> Ctx
updateDevice w h (Ctx ctx) =
    Ctx { ctx | device = classifyDevice { width = w, height = h } }


phones : Ctx -> a -> a -> a
phones (Ctx ctx) phone notPhone =
    case ( ctx.device.class, ctx.device.orientation ) of
        ( Phone, Portrait ) ->
            phone

        _ ->
            notPhone
