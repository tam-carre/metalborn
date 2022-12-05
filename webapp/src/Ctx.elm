module Ctx exposing (Ctx {- OPAQUE -}, init, phones, previousRoute, pushUrl, replaceUrlWithRoute, route, updateDevice, updateRoute)

import Browser.Navigation as Nav
import Element exposing (Device, DeviceClass(..), Orientation(..), classifyDevice)
import Route exposing (Route)
import Url exposing (Url)


type Ctx
    = Ctx Internal


type alias Internal =
    { device : Device
    , nkey : Nav.Key
    , previousRoute : Maybe Route
    , route : Maybe Route
    }


init : Nav.Key -> Ctx
init nkey =
    Ctx
        { device = { class = Desktop, orientation = Landscape }
        , nkey = nkey
        , previousRoute = Nothing
        , route = Nothing
        }


updateDevice : Int -> Int -> Ctx -> Ctx
updateDevice w h (Ctx ctx) =
    Ctx { ctx | device = classifyDevice { width = w, height = h } }


updateRoute : Route -> Ctx -> Ctx
updateRoute newRoute (Ctx ctx) =
    Ctx { ctx | route = Just newRoute, previousRoute = ctx.route }


previousRoute : Ctx -> Maybe Route
previousRoute (Ctx ctx) =
    ctx.previousRoute


phones : Ctx -> a -> a -> a
phones (Ctx ctx) phone notPhone =
    case ( ctx.device.class, ctx.device.orientation ) of
        ( Phone, Portrait ) ->
            phone

        _ ->
            notPhone


pushUrl : Ctx -> Url -> Cmd msg
pushUrl (Ctx { nkey }) =
    Nav.pushUrl nkey << Url.toString


replaceUrlWithRoute : Ctx -> Route -> Cmd msg
replaceUrlWithRoute (Ctx { nkey }) =
    Nav.replaceUrl nkey << Route.url


route : Ctx -> Maybe Route
route (Ctx ctx) =
    ctx.route
