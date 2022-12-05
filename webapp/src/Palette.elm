module Palette exposing (ColorPalette, SizePalette, bgColor, fontColor, fontSize, padding, paddingTop, paddingY, responsive, spacing, theme)

import Ctx exposing (Ctx)
import Element exposing (Attr, Attribute, Color, paddingEach, rgb, rgba)
import Element.Background as Background
import Element.Font as Font


type alias ColorPalette fieldType =
    { accent : fieldType
    , accentMuted : fieldType
    , transparent : fieldType
    , bg : fieldType
    , fg : fieldType
    , muted : fieldType
    , veryMuted : fieldType
    }


type alias SizePalette fieldType =
    { xs : fieldType
    , s : fieldType
    , m : fieldType
    , l : fieldType
    }


responsive : Ctx -> SizePalette a -> SizePalette a
responsive ctx palette =
    Ctx.phones ctx
        { xs = palette.xs -- can't downsize xs
        , s = palette.xs
        , m = palette.s
        , l = palette.m
        }
        palette


theme : ColorPalette Color
theme =
    { accent = rgb 0.933 0.71 0.341
    , accentMuted = rgb 0.698 0.531 0.253
    , transparent = rgba 0 0 0 0
    , bg = rgb 0 0 0
    , fg = rgb 1 1 1
    , muted = rgb 0.7 0.7 0.7
    , veryMuted = rgb 0.2 0.2 0.2
    }


fontColor : ColorPalette (Attr decorative msg)
fontColor =
    internal.mapColorPalette Font.color theme


bgColor : ColorPalette (Attr decorative msg)
bgColor =
    internal.mapColorPalette Background.color theme


spacing : SizePalette (Attribute msg)
spacing =
    internal.mapSizePalette Element.spacing
        { xs = 25
        , s = 50
        , m = 90
        , l = 150
        }


padding : SizePalette (Attribute msg)
padding =
    internal.mapSizePalette Element.padding internal.paddingVals


paddingY : SizePalette (Attribute msg)
paddingY =
    internal.mapSizePalette (Element.paddingXY 0) internal.paddingVals


paddingTop : SizePalette (Attribute msg)
paddingTop =
    internal.mapSizePalette
        (\val ->
            paddingEach
                { top = val
                , bottom = 0
                , right = 0
                , left = 0
                }
        )
        internal.paddingVals


fontSize : SizePalette (Attr decorative msg)
fontSize =
    internal.mapSizePalette Font.size
        { xs = 14
        , s = 18
        , m = 25
        , l = 50
        }


{-| Ensure low-level APIs remain private
-}
internal :
    { mapSizePalette : (a -> b) -> SizePalette a -> SizePalette b
    , mapColorPalette : (a -> b) -> ColorPalette a -> ColorPalette b
    , paddingVals : SizePalette Int
    }
internal =
    { mapSizePalette =
        \f palette ->
            { xs = f palette.xs
            , s = f palette.s
            , m = f palette.m
            , l = f palette.l
            }
    , mapColorPalette =
        \f palette ->
            { accent = f palette.accent
            , accentMuted = f palette.accentMuted
            , transparent = f palette.transparent
            , bg = f palette.bg
            , fg = f palette.fg
            , muted = f palette.muted
            , veryMuted = f palette.veryMuted
            }
    , paddingVals =
        { xs = 10
        , s = 20
        , m = 40
        , l = 80
        }
    }
