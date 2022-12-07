module Page.Home exposing (Model {- OPAQUE -}, Msg {- OPAQUE -}, page)

import API
import API.Gender as Gender
import Accessors exposing (over)
import Anim exposing (fadeInFast, seq, seqAttrs, seqFadeIns)
import Ctx exposing (Ctx)
import Element exposing (Element, centerX, text)
import Fields as F
import Page exposing (Page)
import Palette exposing (paddingY, responsive, spacing)
import Route exposing (CharacterOrigin(..))
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


update : Ctx -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg (Model model) =
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
    seqFadeIns "HOME_PAGE" [ UI.contentColumn ctx ] <|
        [ (seq << UI.narration) <|
            case Ctx.previousRoute ctx of
                Just (Route.Character (InputCharacter name _)) ->
                    "Impressively, the coppermind did contain information about " ++ name ++ ". You consider what to do next."

                Just (Route.Character RandomCharacter) ->
                    "“Intrigued by the unexpected information which has entered your mind, you ponder for a while.”"

                Just Route.CustomProbabilities ->
                    "You revert the coppermind statue back to normal. The warped energy wanes."

                _ ->
                    "“You arrive at the promised location. The rumored coppermind—a statue of Harmony—stands at the center of the site. You touch the strange metalmind. As expected, you are able to sense its contents immediately.”"
        , seqAttrs [ centerX, (responsive ctx paddingY).s ] <|
            Element.column [ UI.contentColumn ctx ]
                [ nameAndGenderInput model
                , UI.actionLink "Tap random memory" (Route.Character RandomCharacter)
                , UI.actionLink "Tamper with statue" Route.CustomProbabilities
                , UI.actionLinkExternal "Investigate site's origin"
                    "https://github.com/tam-carre/metalborn"
                , UI.ending ctx
                ]
        ]


{-| TODO: Once the other pages are done, it is possible this function
might be moved to `UI.elm` and re-used on different pages
-}
nameAndGenderInput : Internal -> Element Msg
nameAndGenderInput model =
    Element.column [ centerX, spacing.s ]
        [ UI.nameInput NameInputted model.requestName
        , genderLinks model
        ]


genderLinks : Internal -> Element msg
genderLinks model =
    let
        genderLink name gender =
            UI.actionLink (Gender.info gender).str <|
                Route.Character (InputCharacter name gender)
    in
    case model.requestName of
        Just name ->
            (fadeInFast [ centerX ] << Element.row [ spacing.s ]) <|
                List.map (genderLink name) [ API.Male, API.Female, API.Other ]

        Nothing ->
            -- Keep the space to use up a layout row with the font's full height
            text " "
