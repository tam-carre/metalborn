module Page exposing (Page)

import Ctx exposing (Ctx)
import Element exposing (Element)


type alias Page deps model msg =
    { title : model -> String
    , init : deps -> ( model, Cmd msg )
    , update : Ctx -> msg -> model -> ( model, Cmd msg )
    , view : Ctx -> model -> Element msg
    }
