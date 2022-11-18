module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as UP


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { title : String
    , key : Nav.Key
    , route : Maybe Route
    }


type Route
    = Home
    | WhereToGet
    | Links


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { title = "Metalborn"
      , key = key
      , route = UP.parse urlToRoute url
      }
    , Cmd.none
    )


urlToRoute : UP.Parser (Route -> a) a
urlToRoute =
    UP.oneOf
        [ UP.map Home UP.top
        ]



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( { model | route = UP.parse urlToRoute url }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )



-- VIEW


view : Model -> Document Msg
view model =
    { title = model.title
    , body = []
    }
