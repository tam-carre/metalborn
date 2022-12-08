module Page.Character exposing (Model {- OPAQUE -}, Msg {- OPAQUE -}, dontRerouteUrlChange, page)

{-| This module is a very early stage work in progress
-}

import API
import Ctx exposing (Ctx)
import Element exposing (Element, fill, width)
import Fields as F
import Page exposing (Page)
import Palette exposing (paddingTop, responsive)
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (CharacterOrigin(..))
import Sequential exposing (seq, seqAttrs, seqFadeIns)
import UI
import Utils exposing (HttpResult, receive)


page : Page (Maybe ( API.Name, API.Gender )) Model Msg
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
    { input : Maybe ( API.Name, API.Gender )
    , character : WebData API.Character
    , urlCopied : Bool
    }


init : Maybe ( API.Name, API.Gender ) -> ( Model, Cmd Msg )
init input =
    ( Model
        { character = Loading
        , input = input
        , urlCopied = False
        }
    , case input of
        Nothing ->
            API.postApiCharacter ( Nothing, Nothing ) CharacterReceived

        Just ( name, gender ) ->
            API.postApiCharacter ( Just name, Just gender ) CharacterReceived
    )


title : Model -> String
title (Model { character }) =
    "Metalborn: "
        ++ (character
                |> RemoteData.unwrap "...tapping memory..." (\(API.Character name _ _ _) -> name)
           )


{-| Contexts in which a URL change should be ignored by Elm
-}
dontRerouteUrlChange : Ctx -> Bool
dontRerouteUrlChange ctx =
    case Ctx.route ctx of
        -- If the url changes while on the route RandomCharacter it's just us
        -- making the random character's URL copyable, we don't actually want
        -- to reroute
        Route.Character RandomCharacter ->
            True

        _ ->
            False



-- UPDATE


type Msg
    = CharacterReceived (HttpResult API.Character)
    | SaveAndCopyClicked


update : Ctx -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg (Model model) =
    case msg of
        CharacterReceived webdata ->
            ( Model (model |> receive F.character webdata)
            , case webdata of
                Ok (API.Character name gender _ _) ->
                    case Ctx.route ctx of
                        Route.Character RandomCharacter ->
                            Ctx.replaceUrlWithRoute ctx <|
                                Route.Character (InputCharacter name gender)

                        _ ->
                            Cmd.none

                Err _ ->
                    Cmd.none
            )

        SaveAndCopyClicked ->
            ( Model { model | urlCopied = True }
            , Ports.copyCurrentUrlToClipboard ()
            )



-- VIEW


view : Ctx -> Model -> Element Msg
view ctx (Model ({ input, character } as model)) =
    seqFadeIns "CHARACTER_PAGE" [ UI.narrationColumn ctx ] <|
        (case input of
            Nothing ->
                [ (seq << UI.narration)
                    "You touch the large coppermind, unsure what you will find in it."
                , (seq << UI.narration)
                    "You feel the coppermind's embrace."
                ]

            Just ( name, _ ) ->
                [ (seq << UI.narration)
                    "You touch the large coppermind, resolute in your search."
                , (seq << UI.narration)
                    (name ++ ". That is who you wish to know about.")
                ]
        )
            ++ [ (seq << UI.narration) "A memory flows into you."
               , (seqAttrs [ width fill ] << UI.load character << UI.viewCharacter ctx) <|
                    Element.column [ UI.contentColumn ctx, (responsive ctx paddingTop).l ]
                        [ if model.urlCopied then
                            UI.mutedBtn "Copied to clipboard" SaveAndCopyClicked

                          else
                            UI.actionBtn "Save and copy information" SaveAndCopyClicked
                        , UI.actionLink "Search new memory" Route.Home
                        , UI.actionLink "Tamper with coppermind" Route.Probabilities
                        , UI.ending ctx
                        ]
               ]
