module Page.Probabilities exposing (Model {- OPAQUE -}, Msg {- OPAQUE -}, page)

import API
import API.Name as Name
import Accessors exposing (A_Lens, get, over, set)
import Browser.Dom exposing (setViewport)
import Ctx exposing (Ctx)
import Element exposing (Element, centerX, centerY, fill, height, px, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Fields as F
import Html.Attributes
import Page exposing (Page)
import Palette exposing (bgColor, padding, paddingTop, paddingY, responsive, spacing, theme)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (CharacterOrigin(..))
import Sequential exposing (Sequential, fadeIn, seq, seqAttrs, seqFadeIns)
import Task
import UI
import Utils exposing (HttpResult, receive)


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
    { probabs : API.AbilityProbabilities
    , requestName : Maybe API.Name
    , character : WebData API.Character
    , clickedTryAgain : Bool
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model
        { probabs = defaultProbabs
        , requestName = Nothing
        , character = NotAsked
        , clickedTryAgain = False
        }
    , Cmd.none
    )


defaultProbabs : API.AbilityProbabilities
defaultProbabs =
    { metalborn = 1
    , twinborn = 0.8
    , fullPower = 0.01
    , spike = 0.05
    , medall = 0.05
    , grenade = 0.25
    }


title : Model -> String
title _ =
    "Metalborn: A Mistborn Era 2 character generator!"



-- UPDATE


type Msg
    = NameTyped String
    | ProbabSelected (API.AbilityProbabilities -> API.AbilityProbabilities)
    | GenderSelected API.Gender
    | CharacterReceived (HttpResult API.Character)
    | TryAgainClicked
    | ScrollToTopRequired


update : Ctx -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg (Model ({ requestName, probabs } as model)) =
    case msg of
        NameTyped newName ->
            ( Model (model |> over F.requestName (Name.processInput newName))
            , Cmd.none
            )

        ProbabSelected probabUpdate ->
            ( Model (model |> over F.probabs probabUpdate)
            , Cmd.none
            )

        GenderSelected gender ->
            ( Model model
            , API.postApiRandomCharacter ( requestName, Just gender, probabs ) CharacterReceived
            )

        CharacterReceived webdata ->
            ( Model (model |> receive F.character webdata)
            , Cmd.none
            )

        TryAgainClicked ->
            ( Model { model | character = NotAsked, clickedTryAgain = True }
            , Task.perform (always ScrollToTopRequired) <| setViewport 0 0
            )

        ScrollToTopRequired ->
            ( Model model, Cmd.none )



-- VIEW


view : Ctx -> Model -> Element Msg
view ctx (Model model) =
    seqFadeIns "PROBABILITIES" [ UI.contentColumn ctx, width fill ] <|
        narration ctx model
            ++ probabSettings ctx model


narration : Ctx -> Internal -> List (Sequential msg)
narration ctx model =
    if model.clickedTryAgain then
        [ (seq << fadeIn [] << UI.narration)
            "Try different settings, name or gender in order to get different results."
        ]

    else
        [ (seq << UI.narration) <|
            case Ctx.previousRoute ctx of
                Just (Route.Character (InputCharacter name _)) ->
                    "Uneasy about having discovered information about " ++ name ++ ", you decide to use your abilities to tamper with the enigmatic coppermind."

                Just (Route.Character RandomCharacter) ->
                    "Unsatisfied with the unnerving results of your experiments, you decide to use your abilities to tamper with the enigmatic coppermind."

                Just Route.Home ->
                    "You came here with a plan, and decide to put it into action."

                _ ->
                    "You activate your powers and approach the statue."
        , (seq << UI.narration)
            "A twisted energy emanates from your hand."
        ]


probabSettings : Ctx -> Internal -> List (Sequential Msg)
probabSettings ctx model =
    [ seqAttrs [ width fill, centerX ] <|
        wrappedRow [ centerX, (responsive ctx paddingY).s, spacing.xs ] <|
            List.map (probabSlider ctx model)
                [ ( "Metalborn", F.metalborn )
                , ( "Twinborn", F.twinborn )
                , ( "Mistborn or Feruchemist", F.fullPower )
                , ( "Hemalurgic spikes", F.spike )
                , ( "Southern Medallions", F.medall )
                , ( "Allomantic Grenade", F.grenade )
                ]
    , seqAttrs [ centerX, (responsive ctx paddingY).s ] <|
        Element.column [ UI.contentColumn ctx ]
            [ case model.character of
                NotAsked ->
                    Element.column [ centerX ]
                        [ UI.nameAndGenderInput (Just GenderSelected)
                            NameTyped
                            model.requestName
                        ]

                _ ->
                    (UI.load model.character << UI.viewCharacter ctx) <|
                        Element.column [ UI.contentColumn ctx, (responsive ctx paddingTop).l ]
                            [ UI.actionBtn "Try again" TryAgainClicked
                            , UI.actionLink "Return coppermind to normal" Route.Home
                            , UI.ending ctx
                            ]
            ]
    ]


probabSlider :
    Ctx
    -> Internal
    -> ( String, A_Lens Never API.AbilityProbabilities API.Probability )
    -> Element Msg
probabSlider ctx model ( prettyName, field ) =
    Element.el
        [ bgColor.fgGlowMuted
        , Border.rounded 7
        , padding.xs
        , Element.htmlAttribute (Html.Attributes.style "marginLeft" "auto")
        , Element.htmlAttribute (Html.Attributes.style "marginRight" "auto")
        ]
        (Input.slider
            [ height << px <| Ctx.phones ctx 20 30
            , width (px 300)
            , centerX
            , Element.behindContent
                (Element.el
                    [ width (px 300)
                    , height << px <| Ctx.phones ctx 13 20
                    , centerY
                    , Background.color theme.fgGlow
                    , Border.rounded 6
                    , Border.width 0
                    ]
                    Element.none
                )
            ]
            { onChange = ProbabSelected << set field
            , label =
                (Input.labelAbove (paddingY.s :: UI.actionStyle) << text) <|
                    prettyName
                        ++ ": "
                        ++ (model
                                |> get (F.probabs << field)
                                |> (*) 100
                                |> round
                                |> String.fromInt
                           )
                        ++ "%"
            , min = 0
            , max = 1
            , value = model |> get (F.probabs << field)
            , thumb =
                Input.thumb
                    [ bgColor.accentMuted
                    , Border.color theme.bg
                    , Border.rounded 6
                    , width (px 60)
                    , height << px <| Ctx.phones ctx 16 24
                    , Border.width 2
                    , Element.pointer
                    , Element.focused
                        [ Border.glow theme.transparent 0
                        ]
                    ]
            , step = Just 0.01
            }
        )
