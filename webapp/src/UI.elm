module UI exposing (actionBtn, actionLink, actionLinkExternal, contentColumn, ending, load, md, mutedBtn, nameInput, narration, narrationColumn)

import Anim exposing (transitionColorHover)
import Ctx exposing (Ctx)
import Element exposing (Element, centerX, centerY, fill, height, paragraph, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (focusedOnLoad)
import Loading exposing (LoaderType(..), defaultConfig)
import Markdown.Parser as MD
import Markdown.Renderer as MDR
import Maybe.Extra as Maybe
import Palette exposing (bgColor, fontColor, padding, paddingTop, responsive, spacing, theme)
import RemoteData exposing (WebData)
import Result.Extra as Result
import Route exposing (Route)


md : String -> Element msg
md input =
    input
        |> MD.parse
        |> Result.mapError (List.map MD.deadEndToString >> String.join "\n")
        |> Result.andThen (MDR.render MDR.defaultHtmlRenderer)
        |> Result.unwrap Nothing List.head
        |> Maybe.unwrap (text private.error) Element.html


narration : String -> Element msg
narration parag =
    paragraph [ Font.italic, centerX ] [ text parag ]


actionLink : String -> Route -> Element msg
actionLink label route =
    Element.link action
        { url = Route.url route, label = text label }


actionBtn : String -> msg -> Element msg
actionBtn label msg =
    Input.button action
        { onPress = Just msg, label = text label }


mutedBtn : String -> msg -> Element msg
mutedBtn label msg =
    Input.button
        (action ++ [ fontColor.mutedMuted, Element.mouseOver [ fontColor.mutedMuted ] ])
        { onPress = Just msg, label = text label }


actionLinkExternal : String -> String -> Element msg
actionLinkExternal label url =
    Element.newTabLink action
        { url = url, label = text label }


nameInput : (String -> msg) -> Maybe String -> Element msg
nameInput inputChangedMsg modelValue =
    Input.text (focusedOnLoad :: action ++ private.discreetInput)
        { onChange = inputChangedMsg
        , text = Maybe.withDefault "" modelValue
        , label = Input.labelHidden "Name"
        , placeholder =
            (Just << Input.placeholder [ fontColor.muted ] << text) "Search name"
        }


action : List (Element.Attribute msg)
action =
    [ centerX
    , Font.bold
    , Font.center
    , fontColor.muted
    , bgColor.transparent
    , Border.width 0
    , Border.rounded 10
    , Element.focused [ Border.glow theme.transparent 0 ]
    , Element.mouseOver [ fontColor.accent ]
    , Element.mouseDown [ fontColor.accent ]
    , Element.pointer
    , transitionColorHover
    ]


contentColumn : Ctx -> Element.Attribute msg
contentColumn ctx =
    (responsive ctx spacing).s


narrationColumn : Ctx -> Element.Attribute msg
narrationColumn ctx =
    (responsive ctx spacing).s


load : (a -> Element msg) -> WebData a -> Element msg
load viewLoaded webdata =
    case webdata of
        RemoteData.Failure _ ->
            narration private.error

        RemoteData.NotAsked ->
            Element.none

        RemoteData.Loading ->
            private.loader

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


private :
    { loader : Element a
    , discreetInput : List (Element.Attr () msg)
    , error : String
    }
private =
    { loader =
        (Element.el [ centerX, centerY, width fill, height fill ] << Element.html) <|
            Loading.render Sonar { defaultConfig | color = "#888" } Loading.On
    , discreetInput =
        [ fontColor.fg
        , padding.s
        , Element.focused [ Border.glow theme.fgGlow 5 ]
        , Element.mouseOver [ fontColor.fg ]
        ]
    , error =
        " [An error occurred. Please contact m.tam.carre at gmail.com] "
    }
