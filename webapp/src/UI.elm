module UI exposing (actionLink, nameInput, narration)

import Anim exposing (transitionColorHover)
import Element exposing (Element, centerX, paragraph, text)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (focusedOnLoad)
import Palette exposing (bgColor, fontColor, padding, paddingY, theme)


narration : String -> Element msg
narration parag =
    paragraph [ Font.italic, centerX, paddingY.s ] [ text parag ]


actionLink : String -> String -> Element msg
actionLink label url =
    Element.link action
        { url = url, label = text label }


nameInput : (String -> msg) -> Maybe String -> Element msg
nameInput inputChangedMsg modelValue =
    Input.text (focusedOnLoad :: action ++ discreetInput)
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
    , transitionColorHover
    ]


discreetInput : List (Element.Attribute msg)
discreetInput =
    [ fontColor.fg
    , padding.s
    , Element.focused [ Border.glow theme.veryMuted 5 ]
    , Element.mouseOver [ fontColor.fg ]
    ]
