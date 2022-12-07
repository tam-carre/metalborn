module Ctx exposing (Ctx {- OPAQUE -}, init, phones, previousRoute, pushUrl, replaceUrlWithRoute, route, updateDevice, updateRoute)

import Browser.Navigation as Nav
import Element exposing (Device, DeviceClass(..), Orientation(..), classifyDevice)
import Route exposing (CharacterOrigin(..), Route)
import Url exposing (Url)


type Ctx
    = Ctx Internal


type alias Internal =
    { device : Device
    , nkey : Nav.Key
    , routeHistory : List Route
    , route : Route
    }


init : Nav.Key -> Url -> Ctx
init nkey url =
    Ctx
        { device = { class = Desktop, orientation = Landscape }
        , nkey = nkey
        , routeHistory = []
        , route = Route.fromUrl url
        }


updateDevice : Int -> Int -> Ctx -> Ctx
updateDevice w h (Ctx ctx) =
    Ctx { ctx | device = classifyDevice { width = w, height = h } }


updateRoute : Url -> Ctx -> Ctx
updateRoute newUrl (Ctx ctx) =
    Ctx
        { ctx
            | route = Route.fromUrl newUrl

            -- We don't need to save any more than the past 2 routes currently
            , routeHistory = ctx.route :: List.take 1 ctx.routeHistory
        }


previousRoute : Ctx -> Maybe Route
previousRoute (Ctx ctx) =
    case ctx.routeHistory of
        -- If the previous route was a specific character, but the route just before *that*
        -- was a random character, it's just us who changed the URL to make it shareable
        (Route.Character (InputCharacter _ _)) :: (Route.Character RandomCharacter) :: _ ->
            Just <| Route.Character RandomCharacter

        _ ->
            List.head ctx.routeHistory


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


route : Ctx -> Route
route (Ctx ctx) =
    ctx.route
