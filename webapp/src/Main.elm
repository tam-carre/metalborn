module Main exposing (Model, Msg, PageModel, main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html)
import Pages.Character
import Pages.Home
import Route exposing (Route(..))
import Url exposing (Url)


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
    { nkey : Nav.Key
    , pageModel : PageModel
    }


type PageModel
    = HomeModel Pages.Home.Model
    | CharacterModel Pages.Character.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url nkey =
    goToRoute nkey <| Route.fromUrl url



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | PageMsg PageMsg


type PageMsg
    = HomeMsg Pages.Home.Msg
    | CharacterMsg Pages.Character.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ nkey } as model) =
    case msg of
        UrlChanged url ->
            goToRoute nkey <| Route.fromUrl url

        LinkClicked urlRequest ->
            ( model
            , case urlRequest of
                Browser.Internal url ->
                    Nav.pushUrl nkey (Url.toString url)

                Browser.External href ->
                    Nav.load href
            )

        PageMsg pageMsg ->
            updatePage model pageMsg


updatePage : Model -> PageMsg -> ( Model, Cmd Msg )
updatePage ({ nkey, pageModel } as model) pageMsg =
    case ( pageMsg, pageModel ) of
        ( HomeMsg homeMsg, HomeModel homeModel ) ->
            fromHome nkey <| Pages.Home.update homeMsg homeModel

        ( CharacterMsg charaMsg, CharacterModel charaModel ) ->
            fromChara nkey <| Pages.Character.update charaMsg charaModel

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    case model.pageModel of
        HomeModel homeModel ->
            { title = Pages.Home.title
            , body = [ htmlFrom HomeMsg <| Pages.Home.view homeModel ]
            }

        CharacterModel charaModel ->
            { title = Pages.Character.title charaModel
            , body = [ htmlFrom CharacterMsg <| Pages.Character.view charaModel ]
            }



-- MISC


goToRoute : Nav.Key -> Route -> ( Model, Cmd Msg )
goToRoute nkey route =
    case route of
        RouteHome ->
            fromHome nkey Pages.Home.init

        RouteCharacter name gender ->
            fromChara nkey <| Pages.Character.init name gender


fromHome : Nav.Key -> ( Pages.Home.Model, Cmd Pages.Home.Msg ) -> ( Model, Cmd Msg )
fromHome nkey =
    toMain nkey HomeModel HomeMsg


fromChara : Nav.Key -> ( Pages.Character.Model, Cmd Pages.Character.Msg ) -> ( Model, Cmd Msg )
fromChara nkey =
    toMain nkey CharacterModel CharacterMsg


htmlFrom : (a -> PageMsg) -> Html a -> Html Msg
htmlFrom toMainMsg =
    Html.map (PageMsg << toMainMsg)


toMain : Nav.Key -> (pageModel -> PageModel) -> (pageMsg -> PageMsg) -> ( pageModel, Cmd pageMsg ) -> ( Model, Cmd Msg )
toMain nkey toModel toMsg ( subModel, subCmd ) =
    ( { nkey = nkey, pageModel = toModel subModel }
    , Cmd.map (PageMsg << toMsg) subCmd
    )
