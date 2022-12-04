module Anim exposing (delayFadeIn, fadeIn, fadeInFast, transitionColorHover)

import Element exposing (Element)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Simple.Transition as Transition exposing (Millis, Option, Property)


fadeIn : List (Element.Attribute msg) -> Element msg -> Element msg
fadeIn =
    internal.fadeIn_ 4000


fadeInFast : List (Element.Attribute msg) -> Element msg -> Element msg
fadeInFast =
    internal.fadeIn_ 2000


delayFadeIn : List (Element.Attribute msg) -> Element msg -> Element msg
delayFadeIn =
    Animation.steps
        { startAt = [ P.opacity 0 ]
        , options = []
        }
        [ Animation.step 1000 [ P.opacity 0 ]
        , Animation.step 2000 [ P.opacity 1 ]
        ]
        |> internal.anim


transitionColorHover : Element.Attribute msg
transitionColorHover =
    internal.transition 500 [ Transition.color ]


{-| Ensure low-level APIs remain private
-}
internal :
    { transition : Millis -> List (Millis -> List Option -> Property) -> Element.Attribute a
    , anim : Animation -> List (Element.Attribute a) -> Element a -> Element a
    , fadeIn_ : Int -> List (Element.Attribute a) -> Element a -> Element a
    }
internal =
    { transition =
        \duration whatAttrs ->
            Element.htmlAttribute (Transition.all { duration = duration, options = [] } whatAttrs)
    , anim =
        Animated.ui
            { behindContent = Element.behindContent
            , htmlAttribute = Element.htmlAttribute
            , html = Element.html
            }
            Element.el
    , fadeIn_ =
        \duration ->
            Animation.fromTo { duration = duration, options = [] }
                [ P.opacity 0 ]
                [ P.opacity 1 ]
                |> internal.anim
    }
