module Route exposing (CharacterOrigin(..), Route(..), fromUrl, url)

import API
import API.Gender as Gender
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as UP exposing ((</>), custom, map, oneOf, s, string, top)


type Route
    = Home
    | Character CharacterOrigin
    | Probabilities


type CharacterOrigin
    = RandomCharacter
    | InputCharacter API.Name API.Gender


{-| Typesafe string-URL builder
-}
url : Route -> String
url route =
    case route of
        Home ->
            Builder.absolute [] []

        Character (InputCharacter name gender) ->
            Builder.absolute [ internal.paths.character, name, (Gender.info gender).str ] []

        Character RandomCharacter ->
            Builder.absolute [ internal.paths.character ] []

        Probabilities ->
            Builder.absolute [ internal.paths.probabilities ] []


fromUrl : Url -> Route
fromUrl =
    Maybe.withDefault Home << UP.parse (oneOf routes)


routes : List (UP.Parser (Route -> a) a)
routes =
    [ map Home top
    , map (\name gender -> Character (InputCharacter name gender))
        (s internal.paths.character </> string </> custom "GENDER" Gender.fromStr)
    , map (Character RandomCharacter) <| s internal.paths.character
    , map Probabilities <| s internal.paths.probabilities
    ]


internal :
    { paths :
        { character : String
        , home : String
        , probabilities : String
        }
    }
internal =
    { paths =
        { character = "character"
        , home = "home"
        , probabilities = "probabilities"
        }
    }
