module Pages.Home exposing (Model, Msg, init, title, update, view)

import API
import Accessors exposing (set)
import Html exposing (Html)
import Lenses as L
import Utils exposing (noCmd)



-- MODEL


type alias Model =
    { requestName : Maybe API.Name
    , requestGender : Maybe API.Gender
    }


init : ( Model, Cmd Msg )
init =
    noCmd
        { requestName = Nothing
        , requestGender = Nothing
        }



-- UPDATE


type Msg
    = RequestNameEdited String
    | RequestGenderEdited API.Gender


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        RequestNameEdited name ->
            noCmd << set L.requestName (Just name)

        RequestGenderEdited gender ->
            noCmd << set L.requestGender (Just gender)



-- VIEW


title : String
title =
    "Metalborn: A Mistborn Era 2 character generator!"


view : Model -> Html Msg
view _ =
    Html.text "Home"
