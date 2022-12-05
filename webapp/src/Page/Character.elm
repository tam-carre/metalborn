module Page.Character exposing (Model {- OPAQUE -}, Msg {- OPAQUE -}, page)

{-| This module is a very early stage work in progress
-}

import API
import API.Gender as Gender
import Accessors exposing (set)
import Anim exposing (seq, seqAttrs, seqFadeIns)
import Ctx exposing (Ctx)
import Element exposing (Element, centerX, column, fill, text, width)
import Fields as F
import Page exposing (Page)
import Palette exposing (fontColor, fontSize, paddingTop, paddingY, responsive)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import UI
import Utils exposing (HttpResult, noCmd, receive)


page : Page Model Msg (Maybe ( API.Name, API.Gender ))
page =
    { title = title
    , init = init
    , update = update
    , view = view
    }



-- MODEL


type Model
    = Model Internal


type alias Internal =
    { input : Maybe ( API.Name, API.Gender )
    , character : WebData API.Character
    }


init : Maybe ( API.Name, API.Gender ) -> ( Model, Cmd Msg )
init input =
    ( Model { character = Loading, input = input }
    , case input of
        Nothing ->
            API.postApiCharacter ( Nothing, Nothing ) CharacterReceived

        Just ( name, gender ) ->
            API.postApiCharacter ( Just name, Just gender ) CharacterReceived
    )


title : Model -> String
title (Model { character }) =
    "Metalborn: "
        ++ (character
                |> RemoteData.unwrap "...tapping memory..." (\(API.Character name _ _ _) -> name)
           )



-- UPDATE


type Msg
    = CharacterReceived (HttpResult API.Character)


update : Ctx -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg (Model model) =
    case msg of
        CharacterReceived webdata ->
            Tuple.second
                ( (noCmd << Model << receive F.character webdata) model
                , ( Model
                        (set F.character
                            (Success <|
                                API.Character "Yagoo"
                                    API.Male
                                    (API.Abilities (Just API.Fullborn)
                                        { spikedA = [ API.Steel ]
                                        , spikedF = []
                                        , medallF = []
                                        , grenade = False
                                        }
                                    )
                                    [ API.AllomancyBlock "**Yagoo** can use Allomantic **Copper**, making him a **Smoker**. **Smokers** can protect nearby Allomancers from being detected by Seekers (Bronze Allomancers). In addition, a Smoker is immune to emotional Allomancy while burning Copper. It is said to be possible for Smokers to shield others from emotional Allomancy, but the requirements to achieve this feat are not well known."
                                    , API.FeruchemyBlock "Moreover, **Yagoo** can use Feruchemical **Duralumin**, making him a **Connector**. **Connectors** can store Connection. Filling a duraluminmind can be used to reduce other people's awareness of and friendship with the Connector, as these Spiritual Connections become stored away. Tapping it would strengthen Connections or allow the Connector to form relationships faster. Southern Scadrians use unsealed duraluminminds to enable them to communicate with others on foreign lands."
                                    , API.TwinbornBlock "Some call the combination of a Smoker and Connector a **Shelter**. **Shelters** are Allomancers' best friends. Twinborn combinations create subtle new effects which are not well known at this time."
                                    , API.SpikesBlock "Yagoo, after defeating Hemalurgists, came into possession of **1 Hemalurgic spike**, granting him access to, or a boost of power in, Allomantic Electrum."
                                    ]
                            )
                            model
                        )
                  , case webdata of
                        Ok (API.Character name gender _ _) ->
                            if Ctx.route ctx == Just (Route.Character name gender) then
                                Cmd.none

                            else
                                Ctx.replaceUrlWithRoute ctx <| Route.Character name gender

                        Err _ ->
                            Cmd.none
                  )
                )



-- VIEW


view : Ctx -> Model -> Element Msg
view ctx (Model { input, character }) =
    seqFadeIns "CHARACTER_PAGE" [ UI.narrationColumn ctx ] <|
        (case input of
            Nothing ->
                [ (seq << UI.narration)
                    "You touch the large coppermind, unsure what you will find in it."
                , (seq << UI.narration)
                    "You feel the coppermind's embrace."
                ]

            Just ( name, _ ) ->
                [ (seq << UI.narration)
                    "You touch the large coppermind, resolute in your search."
                , (seq << UI.narration)
                    (name ++ ". That is who you wish to know about.")
                ]
        )
            ++ [ (seq << UI.narration) "A memory flows into you."
               , (seqAttrs [ width fill ] << UI.load (viewCharacter ctx)) character
               ]


viewCharacter : Ctx -> API.Character -> Element msg
viewCharacter ctx (API.Character name gender (API.Abilities _ _) descriptionBlocks) =
    seqFadeIns "CHARACTER" [ width fill, (responsive ctx paddingY).s ] <|
        [ seqAttrs
            [ (responsive ctx paddingY).s
            , (responsive ctx fontSize).m
            , fontColor.accentMuted
            , centerX
            ]
            (text (name ++ " " ++ (Gender.info gender).symbol))
        , (seq << column [ centerX, (responsive ctx paddingY).s ]) <|
            List.map viewDescriptionBlock descriptionBlocks
        , (seqAttrs [ centerX ] << Element.column [ UI.contentColumn ctx, (responsive ctx paddingTop).l ])
            [ UI.actionLink "Search new name" Route.Home
            , UI.actionLink "Tamper with coppermind" Route.CustomProbabilities
            , UI.ending ctx
            ]
        ]


viewDescriptionBlock : API.DescriptionBlock -> Element msg
viewDescriptionBlock block =
    let
        -- Note: in a future iteration we might style each type of block differently
        content =
            case block of
                API.AllomancyBlock b ->
                    b

                API.FeruchemyBlock b ->
                    b

                API.TwinbornBlock b ->
                    b

                API.SpikesBlock b ->
                    b

                API.MedallionBlock b ->
                    b

                API.GrenadeBlock b ->
                    b
    in
    Element.paragraph [ fontColor.muted ] <| [ UI.md content ]
