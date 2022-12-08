module UI exposing (actionBtn, actionLink, actionLinkExternal, actionStyle, contentColumn, ending, load, mutedBtn, nameAndGenderInput, narration, narrationColumn, viewCharacter)

import API
import API.Gender as Gender
import Ctx exposing (Ctx)
import Element exposing (Element, centerX, centerY, column, fill, height, paragraph, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (focusedOnLoad)
import Loading exposing (LoaderType(..), defaultConfig)
import Markdown.Parser as MD
import Markdown.Renderer as MDR
import Maybe.Extra as Maybe
import Palette exposing (bgColor, fontColor, fontSize, padding, paddingTop, paddingY, responsive, spacing, theme)
import RemoteData exposing (WebData)
import Result.Extra as Result
import Route exposing (CharacterOrigin(..), Route)
import Sequential exposing (fadeIn, seq, seqAttrs, seqFadeIns)
import Simple.Transition as Transition


narration : String -> Element msg
narration parag =
    paragraph [ Font.italic, centerX ] [ text parag ]


actionLink : String -> Route -> Element msg
actionLink label route =
    Element.link internal.action
        { url = Route.url route, label = text label }


actionBtn : String -> msg -> Element msg
actionBtn label msg =
    Input.button internal.action
        { onPress = Just msg, label = text label }


mutedBtn : String -> msg -> Element msg
mutedBtn label msg =
    Input.button
        (internal.action ++ [ fontColor.mutedMuted, Element.mouseOver [ fontColor.mutedMuted ] ])
        { onPress = Just msg, label = text label }


actionLinkExternal : String -> String -> Element msg
actionLinkExternal label url =
    Element.newTabLink internal.action
        { url = url, label = text label }


actionStyle : List (Element.Attribute msg)
actionStyle =
    [ centerX
    , Font.bold
    , Font.center
    , fontColor.muted
    , bgColor.transparent
    , Border.width 0
    , Border.rounded 10
    , Element.focused [ Border.glow theme.transparent 0 ]
    ]


contentColumn : Ctx -> Element.Attribute msg
contentColumn ctx =
    (responsive ctx spacing).s


narrationColumn : Ctx -> Element.Attribute msg
narrationColumn ctx =
    (responsive ctx spacing).s


viewCharacter : Ctx -> Element msg -> API.Character -> Element msg
viewCharacter ctx links (API.Character name gender (API.Abilities _ _) descriptionBlocks) =
    let
        md =
            MD.parse
                >> Result.mapError (List.map MD.deadEndToString >> String.join "\n")
                >> Result.andThen (MDR.render MDR.defaultHtmlRenderer)
                >> Result.unwrap Nothing List.head
                >> Maybe.unwrap (text internal.error) Element.html

        -- Note: in a future iteration we might style each type of block differently
        viewDescriptionBlock block =
            let
                content =
                    case block of
                        API.AllomancyBlock b ->
                            b

                        API.FeruchemyBlock b ->
                            b

                        API.TwinbornBlock b ->
                            b

                        API.SpikesBlock b ->
                            b

                        API.MedallionBlock b ->
                            b

                        API.GrenadeBlock b ->
                            b
            in
            Element.paragraph [ fontColor.muted ] <| [ md content ]
    in
    seqFadeIns "CHARACTER" [ width fill, (responsive ctx paddingY).s ] <|
        [ seqAttrs
            [ (responsive ctx paddingY).s
            , (responsive ctx fontSize).m
            , fontColor.accentMuted
            , centerX
            ]
            (text (name ++ " " ++ (Gender.info gender).symbol))
        , (seq << column [ centerX, (responsive ctx paddingY).s ]) <|
            List.map viewDescriptionBlock descriptionBlocks
        , seqAttrs [ centerX ] links
        ]


load : WebData a -> (a -> Element msg) -> Element msg
load webdata viewLoaded =
    case webdata of
        RemoteData.Failure _ ->
            narration internal.error

        RemoteData.NotAsked ->
            Element.none

        RemoteData.Loading ->
            (Element.el [ centerX, centerY, width fill, height fill ] << Element.html) <|
                Loading.render Sonar { defaultConfig | color = "#888" } Loading.On

        RemoteData.Success data ->
            viewLoaded data


{-| The footer is stylishly used at the end of certain seqFadeIns chains, and hence must
be manually included by each page
-}
ending : Ctx -> Element msg
ending ctx =
    Element.el
        [ centerX
        , (responsive ctx paddingTop).l
        , width fill
        ]
        (Element.image [ width fill ]
            { src = "/img/ending.png", description = "Decorative footer image" }
        )


nameAndGenderInput : Maybe (API.Gender -> msg) -> (String -> msg) -> Maybe API.Name -> Element msg
nameAndGenderInput genderMsg nameInputMsg nameUserInput =
    let
        nameInput inputChangedMsg modelValue =
            Input.text
                (focusedOnLoad
                    :: internal.action
                    ++ [ fontColor.fg
                       , padding.s
                       , Element.focused [ Border.glow theme.fgGlow 5 ]
                       , Element.mouseOver [ fontColor.fg ]
                       ]
                )
                { onChange = inputChangedMsg
                , text = Maybe.withDefault "" modelValue
                , label = Input.labelHidden "Name"
                , placeholder =
                    (Just << Input.placeholder [ fontColor.muted ] << text) "Search name"
                }

        genderLink name gender =
            case genderMsg of
                Nothing ->
                    actionLink (Gender.info gender).str <|
                        Route.Character (InputCharacter name gender)

                Just msg ->
                    actionBtn (Gender.info gender).str (msg gender)

        genderLinks =
            case nameUserInput of
                Just name ->
                    (fadeIn [ centerX ] << Element.row [ spacing.s ]) <|
                        List.map (genderLink name) [ API.Male, API.Female, API.Other ]

                Nothing ->
                    -- Keep the space to use up a layout row with the font's full height
                    text " "
    in
    Element.column [ centerX, spacing.s ]
        [ nameInput nameInputMsg nameUserInput
        , genderLinks
        ]


{-| Ensure low-level APIs remain private
-}
internal :
    { error : String
    , action : List (Element.Attribute msg)
    }
internal =
    { error = " [An error occurred. Please contact m.tam.carre at gmail.com] "
    , action =
        actionStyle
            ++ [ Element.mouseOver [ fontColor.accent ]
               , Element.mouseDown [ fontColor.accent ]
               , Element.pointer
               , Element.htmlAttribute
                    (Transition.all { duration = 500, options = [] } [ Transition.color ])
               ]
    }
