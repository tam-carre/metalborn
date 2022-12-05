module Utils exposing (HttpResult, addCmd, noCmd, receive)

import Accessors exposing (A_Lens, set)
import Http
import RemoteData as RD exposing (RemoteData)


type alias HttpResult a =
    Result Http.Error a


noCmd : model -> ( model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd newCmd ( model, cmd ) =
    ( model, Cmd.batch [ cmd, newCmd ] )


{-| Store an API response inside an arbitrary model. `field` parameter dictates where in the model.
-}
receive : A_Lens pr model (RemoteData err val) -> Result err val -> model -> model
receive field =
    set field << RD.fromResult
