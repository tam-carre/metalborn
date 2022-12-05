module Route exposing (Route(..), fromUrl, url)

import API
import API.Gender as Gender
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as UP exposing ((</>), custom, map, oneOf, s, string, top)


type Route
    = Home
    | Character API.Name API.Gender
    | RandomCharacter
    | CustomProbabilities


{-| Typesafe string-URL builder
-}
url : Route -> String
url route =
    case route of
        Home ->
            Builder.absolute [] []

        Character name gender ->
            Builder.absolute [ internal.paths.character, name, (Gender.info gender).str ] []

        RandomCharacter ->
            Builder.absolute [ internal.paths.character ] []

        CustomProbabilities ->
            Builder.absolute [ internal.paths.customProbabilities ] []


fromUrl : Url -> Route
fromUrl =
    Maybe.withDefault Home << UP.parse (oneOf routes)


routes : List (UP.Parser (Route -> a) a)
routes =
    [ map Home top
    , map Character <| s "character" </> string </> custom "GENDER" Gender.fromStr
    , map RandomCharacter <| s internal.paths.character
    , map CustomProbabilities <| s internal.paths.customProbabilities
    ]


internal :
    { paths :
        { character : String
        , home : String
        , customProbabilities : String
        }
    }
internal =
    { paths =
        { character = "character"
        , home = "home"
        , customProbabilities = "custom_probabilities"
        }
    }
