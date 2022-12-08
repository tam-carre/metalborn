module Sequential exposing (Sequential, fadeIn, seq, seqAttrs, seqFadeIns)

import Element exposing (Element)
import Element.Keyed as Keyed
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P


{-| Elements inside `seqFadeIns`; created witn `seq` and `seqAttrs`
Sequentials fade in sequentially (one after the other)
-}
type Sequential msg
    = Internal ( List (Element.Attribute msg), Element msg )


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
            (\i (Internal ( elAttrs, el )) ->
                ( key ++ String.fromInt i
                , internal.delayFadeIn (i * 1000) 2000 elAttrs el
                )
            )


seqAttrs : List (Element.Attribute msg) -> Element msg -> Sequential msg
seqAttrs attrs el =
    Internal ( attrs, el )


seq : Element msg -> Sequential msg
seq =
    seqAttrs []


{-| Manual escape hatch from `seqFadeIns`, sometimes useful
-}
fadeIn : List (Element.Attribute msg) -> Element msg -> Element msg
fadeIn =
    internal.fadeIn_ 2000


{-| Ensure low-level APIs remain private
-}
internal :
    { anim : Animation -> List (Element.Attribute msg) -> Element msg -> Element msg
    , fadeIn_ : Int -> List (Element.Attribute msg) -> Element msg -> Element msg
    , delayFadeIn : Animation.Millis -> Animation.Millis -> List (Element.Attribute msg) -> Element msg -> Element msg
    }
internal =
    { anim =
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
    , delayFadeIn =
        \delay duration ->
            Animation.steps
                { startAt = [ P.opacity 0 ]
                , options = []
                }
                [ Animation.step delay [ P.opacity 0 ]
                , Animation.step duration [ P.opacity 1 ]
                ]
                |> internal.anim
    }
