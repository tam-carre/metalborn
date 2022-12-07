module Main exposing (main)

import Accessors exposing (over)
import Browser exposing (Document)
import Browser.Dom exposing (Viewport, getViewport, setViewport)
import Browser.Events
import Browser.Navigation as Nav
import Ctx exposing (Ctx)
import Element exposing (Element, alignTop, centerX, fill, maximum, text, width)
import Element.Background as Background
import Element.Font as Font
import Fields as F
import Html exposing (Html)
import Page exposing (Page)
import Page.Character as Character
import Page.Home as Home
import Palette exposing (bgColor, fontColor, fontSize, padding, paddingY, responsive, spacing, theme)
import Route exposing (CharacterOrigin(..))
import Task
import Url exposing (Url)
import Utils exposing (addCmd, noCmd)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = always (Browser.Events.onResize ViewportResized)
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { ctx : Ctx
    , pageModel : PageModel
    }


type PageModel
    = HomeModel Home.Model
    | CharacterModel Character.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url nkey =
    { ctx = Ctx.init nkey url
    , pageModel = (HomeModel << Tuple.first) <| Home.page.init ()
    }
        |> goToRouteFromUrl url
        |> addCmd (Task.perform ViewportQueried getViewport)



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ViewportReset
    | ViewportResized Int Int
    | ViewportQueried Viewport
    | PageMsg PageMsg


type PageMsg
    = HomeMsg Home.Msg
    | CharacterMsg Character.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        UrlChanged url ->
            goToRouteFromUrl url

        LinkClicked urlRequest ->
            loadPage urlRequest

        ViewportReset ->
            noCmd

        ViewportResized w h ->
            updateDevice w h

        ViewportQueried { viewport } ->
            updateDevice (round viewport.width) (round viewport.height)

        PageMsg pageMsg ->
            updatePage pageMsg


goToRouteFromUrl : Url -> Model -> ( Model, Cmd Msg )
goToRouteFromUrl url model =
    let
        scrollToTop =
            (addCmd << Task.perform (always ViewportReset)) <| setViewport 0 0
    in
    model
        |> over F.ctx (Ctx.updateRoute url)
        |> (case Route.fromUrl url of
                Route.Home ->
                    scrollToTop << homeToMain (Home.page.init ())

                Route.Character (InputCharacter name gender) ->
                    if Character.dontRerouteUrlChange model.ctx then
                        noCmd

                    else
                        scrollToTop << charaToMain (Character.page.init (Just ( name, gender )))

                Route.Character RandomCharacter ->
                    scrollToTop << charaToMain (Character.page.init Nothing)

                Route.CustomProbabilities ->
                    Debug.todo "not created yet"
           )


loadPage : Browser.UrlRequest -> Model -> ( Model, Cmd msg )
loadPage urlRequest model =
    ( model
    , case urlRequest of
        Browser.Internal url ->
            Ctx.pushUrl model.ctx url

        Browser.External href ->
            Nav.load href
    )


updateDevice : Int -> Int -> Model -> ( Model, Cmd msg )
updateDevice w h =
    noCmd << over F.ctx (Ctx.updateDevice w h)


updatePage : PageMsg -> Model -> ( Model, Cmd Msg )
updatePage pageMsg ({ pageModel } as model) =
    (case ( pageMsg, pageModel ) of
        ( HomeMsg homeMsg, HomeModel homeModel ) ->
            homeToMain (Home.page.update model.ctx homeMsg homeModel)

        ( CharacterMsg charaMsg, CharacterModel charaModel ) ->
            charaToMain (Character.page.update model.ctx charaMsg charaModel)

        _ ->
            noCmd
    )
        model


homeToMain : ( Home.Model, Cmd Home.Msg ) -> Model -> ( Model, Cmd Msg )
homeToMain =
    toMain HomeModel HomeMsg


charaToMain : ( Character.Model, Cmd Character.Msg ) -> Model -> ( Model, Cmd Msg )
charaToMain =
    toMain CharacterModel CharacterMsg


toMain : (pageModel -> PageModel) -> (pageMsg -> PageMsg) -> ( pageModel, Cmd pageMsg ) -> Model -> ( Model, Cmd Msg )
toMain toMainModel toMainMsg ( subModel, subCmd ) model =
    ( { model | pageModel = toMainModel subModel }
    , Cmd.map (PageMsg << toMainMsg) subCmd
    )



-- VIEW


view : Model -> Document Msg
view ({ ctx } as model) =
    case model.pageModel of
        HomeModel mdl ->
            render ctx HomeMsg Home.page mdl

        CharacterModel mdl ->
            render ctx CharacterMsg Character.page mdl


render : Ctx -> (msg -> PageMsg) -> Page model msg deps -> model -> Document Msg
render ctx toMainMsg page mdl =
    { title = page.title mdl
    , body = [ Html.map (PageMsg << toMainMsg) << siteLayout ctx <| page.view ctx mdl ]
    }


siteLayout : Ctx -> Element msg -> Html msg
siteLayout ctx content =
    let
        ( fontSizeResp, paddingResp, spacingResp ) =
            ( responsive ctx fontSize, responsive ctx padding, responsive ctx spacing )

        siteHeader =
            Element.column
                [ centerX
                , alignTop
                , paddingY.m
                , Background.uncropped "/img/ettmetalLogo.png"
                , Font.shadow { offset = ( 0, 0 ), blur = 8, color = theme.bg }
                ]
                [ (Element.el [ fontSizeResp.l, Font.bold, centerX, paddingY.xs ] << text)
                    "METALBORN.IO"
                , Element.el [ fontSizeResp.m, centerX, paddingY.s ] <|
                    Element.paragraph [ Font.center ]
                        [ text "A "
                        , Element.el [ fontColor.accent ] <| text "Mistborn Era 2"
                        , text " character generator"
                        ]
                ]

        -- Downloaded in index.html
        siteFont =
            Font.family
                [ Font.typeface "Lato"
                , Font.sansSerif
                ]
    in
    Element.layout [ bgColor.bg, fontColor.fg, paddingResp.m, siteFont ] <|
        Element.column [ centerX, spacingResp.s ]
            [ Element.link [ centerX ] { url = "/", label = siteHeader }
            , Element.el
                [ fontSizeResp.s, paddingY.m, width (maximum 700 fill) ]
                content
            ]
