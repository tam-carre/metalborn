module Route exposing (Route(..), fromUrl)

import API
import Gender
import Url exposing (Url)
import Url.Parser as UP exposing ((</>))


type Route
    = RouteHome
    | RouteCharacter API.Name API.Gender


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault RouteHome <|
        UP.parse (UP.oneOf routes) url


routes : List (UP.Parser (Route -> a) a)
routes =
    [ UP.map RouteCharacter <|
        UP.s "character"
            </> UP.string
            </> UP.custom "GENDER" Gender.fromStr
    ]
