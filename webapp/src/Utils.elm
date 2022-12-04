module Utils exposing (noCmd, receive)

import Accessors exposing (A_Lens, set)
import RemoteData as RD exposing (RemoteData)


noCmd : model -> ( model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


{-| Store an API response inside an arbitrary model. `field` parameter dictates where in the model.
-}
receive : A_Lens pr model (RemoteData err val) -> Result err val -> model -> model
receive field =
    set field << RD.fromResult
