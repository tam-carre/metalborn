module Pages.Character exposing (Model, Msg, init, title, update, view)

import API
import Gender
import Html exposing (Html, text)
import Http
import Lenses as L
import RemoteData exposing (RemoteData(..), WebData)
import Utils exposing (noCmd, receive)



-- MODEL


type alias Model =
    { name : API.Name
    , gender : API.Gender
    , character : WebData API.Character
    }


init : API.Name -> API.Gender -> ( Model, Cmd Msg )
init name gender =
    ( { name = name
      , gender = gender
      , character = NotAsked
      }
    , API.postApiCharacter ( Just name, Just gender ) CharacterReceived
    )



-- UPDATE


type Msg
    = CharacterReceived (Result Http.Error API.Character)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        CharacterReceived webdata ->
            noCmd << receive L.character webdata



-- VIEW


title : Model -> String
title { name, gender } =
    "Metalborn: " ++ name ++ " (" ++ Gender.toLetter gender ++ " )"


view : Model -> Html Msg
view { character } =
    case character of
        NotAsked ->
            text "Loading..."

        Loading ->
            text "Loading..."

        Failure _ ->
            text "An error occurred."

        Success (API.Character name gender (API.Abilities _ _) _) ->
            text <|
                name
                    ++ Gender.toLetter gender
