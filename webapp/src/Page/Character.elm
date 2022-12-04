module Page.Character exposing (Model {- OPAQUE -}, Msg {- OPAQUE -}, page)

{-| This module is a very early stage work in progress
-}

import API
import API.Gender as Gender
import Accessors exposing (set)
import Ctx exposing (Ctx)
import Element exposing (Element, el, text)
import Fields as F
import Http
import Maybe.Extra as Maybe
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Utils exposing (noCmd, receive)


page : Page Model Msg (Maybe ( API.Name, API.Gender ))
page =
    { title = title
    , init = init
    , update = update
    , view = view
    }



-- MODEL


type Model
    = Model Internal


type alias Internal =
    { random : Bool
    , character : WebData API.Character
    }


init : Maybe ( API.Name, API.Gender ) -> ( Model, Cmd Msg )
init nameAndGender =
    ( Model
        { random = not <| Maybe.isJust nameAndGender
        , character = Loading
        }
    , case nameAndGender of
        Nothing ->
            API.postApiCharacter ( Nothing, Nothing ) CharacterReceived

        Just ( name, gender ) ->
            API.postApiCharacter ( Just name, Just gender ) CharacterReceived
    )


title : Model -> String
title (Model { character }) =
    "Metalborn: "
        ++ (character
                |> RemoteData.map
                    (\(API.Character name gender _ _) ->
                        name ++ " (" ++ .letter (Gender.info gender) ++ " )"
                    )
                |> RemoteData.withDefault "...tapping memory..."
           )



-- UPDATE


type Msg
    = CharacterReceived (Result Http.Error API.Character)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        CharacterReceived webdata ->
            Tuple.second
                ( (noCmd << Model << receive F.character webdata) model
                , (noCmd
                    << Model
                    << set F.character
                        (Success <|
                            API.Character "Kaladin"
                                API.Male
                                (API.Abilities (Just API.Fullborn)
                                    { spikedA = [ API.Steel ]
                                    , spikedF = []
                                    , medallF = []
                                    , grenade = False
                                    }
                                )
                                [ API.AllomancyBlock "Kaladin is a Fullborn." ]
                        )
                  )
                    model
                )



-- VIEW


view : Ctx -> Model -> Element Msg
view _ (Model { character }) =
    el [] (RemoteData.unwrap Element.none viewCharacter character)


viewCharacter : API.Character -> Element msg
viewCharacter (API.Character name gender (API.Abilities _ _) _) =
    text <| name ++ .letter (Gender.info gender)
