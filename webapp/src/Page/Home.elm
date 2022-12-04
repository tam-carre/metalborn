module Page.Home exposing (Model {- OPAQUE -}, Msg {- OPAQUE -}, page)

import API
import API.Gender as Gender
import Accessors exposing (over)
import Anim exposing (delayFadeIn, fadeIn, fadeInFast)
import Ctx exposing (Ctx)
import Element exposing (Element, centerX, text)
import Fields as F
import Page exposing (Page)
import Palette exposing (responsive, spacing)
import UI
import Utils exposing (noCmd)


page : Page Model Msg ()
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
    { requestName : Maybe API.Name }


init : () -> ( Model, Cmd Msg )
init () =
    noCmd <| Model { requestName = Nothing }


title : Model -> String
title _ =
    "Metalborn: A Mistborn Era 2 character generator!"



-- UPDATE


type Msg
    = NameInputted String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        NameInputted newName ->
            ( model |> over F.requestName (processNameInput newName) |> Model
            , Cmd.none
            )


processNameInput : String -> Maybe String -> Maybe String
processNameInput newName oldName =
    if String.startsWith " " newName then
        oldName

    else if newName == "" then
        Nothing

    else
        Just newName



-- VIEW


view : Ctx -> Model -> Element Msg
view ctx (Model model) =
    Element.column [ (responsive ctx spacing).m ]
        [ (fadeIn [] << UI.narration)
            "“You arrive at the promised location. The rumored coppermind—a statue of Harmony—stands at the center. You touch the strange metalmind. As expected, you are able to sense its contents immediately.”"
        , nameAndGenderInput model
        , delayFadeIn [ centerX ] <| UI.actionLink "Tap random memory" "/character"
        , delayFadeIn [ centerX ] <| UI.actionLink "Tamper with statue" "/custom_probabilities"
        ]


{-| TODO: Once the other pages are done, it is possible this function
might be moved to `UI.elm` and re-used on different pages
-}
nameAndGenderInput : Internal -> Element Msg
nameAndGenderInput model =
    Element.column [ centerX, spacing.s ]
        [ delayFadeIn [ centerX ] <| UI.nameInput NameInputted model.requestName
        , genderLinks model
        ]


genderLinks : Internal -> Element msg
genderLinks model =
    let
        genderLink name gender =
            UI.actionLink (Gender.info gender).str
                ("/character/" ++ name ++ "/" ++ (Gender.info gender).str)
    in
    case model.requestName of
        Just name ->
            (fadeInFast [ centerX ] << Element.row [ spacing.s ]) <|
                List.map (genderLink name) [ API.Male, API.Female, API.Other ]

        Nothing ->
            -- Keep the space to use up a layout row with the font's full height
            text " "
