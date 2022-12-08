module Page.Home exposing (Model {- OPAQUE -}, Msg {- OPAQUE -}, page)

import API
import API.Name as Name
import Accessors exposing (over)
import Ctx exposing (Ctx)
import Element exposing (Element, centerX)
import Fields as F
import Page exposing (Page)
import Palette exposing (paddingY, responsive)
import Route exposing (CharacterOrigin(..))
import Sequential exposing (seq, seqAttrs, seqFadeIns)
import UI
import Utils exposing (noCmd)


page : Page () Model Msg
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
    = NameTyped String


update : Ctx -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg (Model model) =
    case msg of
        NameTyped newName ->
            ( Model (model |> over F.requestName (Name.processInput newName))
            , Cmd.none
            )



-- VIEW


view : Ctx -> Model -> Element Msg
view ctx (Model model) =
    seqFadeIns "HOME_PAGE" [ UI.contentColumn ctx ] <|
        [ (seq << UI.narration) <|
            case Ctx.previousRoute ctx of
                Just (Route.Character (InputCharacter name _)) ->
                    "Impressively, the coppermind did contain information about " ++ name ++ ". You consider what to do next."

                Just (Route.Character RandomCharacter) ->
                    "Intrigued by the unexpected information which has entered your mind, you ponder for a while."

                Just Route.Probabilities ->
                    "You revert the coppermind statue back to normal. The warped energy wanes."

                _ ->
                    "You arrive at the promised location. The rumored coppermind—a statue of Harmony—stands at the center of the site. You touch the strange metalmind. As expected, you are able to sense its contents immediately."
        , seqAttrs [ centerX, (responsive ctx paddingY).s ] <|
            Element.column [ UI.contentColumn ctx ]
                [ UI.nameAndGenderInput Nothing NameTyped model.requestName
                , UI.actionLink "Tap random memory" (Route.Character RandomCharacter)
                , UI.actionLink "Tamper with statue" Route.Probabilities
                , UI.actionLinkExternal "Investigate site's origin"
                    "https://github.com/tam-carre/metalborn"
                , UI.ending ctx
                ]
        ]
