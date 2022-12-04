module Page exposing (Page)

import Ctx exposing (Ctx)
import Element exposing (Element)


type alias Page model msg deps =
    { title : model -> String
    , init : deps -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : Ctx -> model -> Element msg
    }
