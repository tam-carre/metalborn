module Anim exposing (Sequential, fadeInFast, seq, seqAttrs, seqFadeIns, transitionColorHover)

import Element exposing (Element)
import Element.Keyed as Keyed
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Simple.Transition as Transition exposing (Millis, Option, Property)


fadeInFast : List (Element.Attribute msg) -> Element msg -> Element msg
fadeInFast =
    internal.fadeIn_ 2000


{-| Elements inside `seqFadeIns`; created witn `seq` and `seqAttrs`
-}
type Sequential msg
    = SequentialINTERNAL ( List (Element.Attribute msg), Element msg )


{-| A unique key is necessary in order to force the animation to play on page change (if you go from one page to another with the same DOM structure but different content, the animation won't play)

Further, note that this takes a list of Sequentials, which are constructed with `seq` and `seqAttrs`.

-}
seqFadeIns :
    String
    -> List (Element.Attribute msg)
    -> List (Sequential msg)
    -> Element msg
seqFadeIns key attrs =
    Keyed.column attrs
        << List.indexedMap
            (\i (SequentialINTERNAL ( elAttrs, el )) ->
                ( key ++ String.fromInt i
                , delayFadeIn_ (i * 1000) 2000 elAttrs el
                )
            )


seqAttrs : List (Element.Attribute msg) -> Element msg -> Sequential msg
seqAttrs attrs el =
    SequentialINTERNAL ( attrs, el )


seq : Element msg -> Sequential msg
seq =
    seqAttrs []


delayFadeIn_ : Int -> Int -> List (Element.Attribute msg) -> Element msg -> Element msg
delayFadeIn_ delay duration =
    Animation.steps
        { startAt = [ P.opacity 0 ]
        , options = []
        }
        [ Animation.step delay [ P.opacity 0 ]
        , Animation.step duration [ P.opacity 1 ]
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
