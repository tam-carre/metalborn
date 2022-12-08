module API.Name exposing (processInput)

import API


{-| When a name is input, decides whether to accept it, keep the previous input, or clear the input value
-}
processInput : String -> Maybe API.Name -> Maybe API.Name
processInput newName oldName =
    if String.startsWith " " newName then
        oldName

    else if newName == "" then
        Nothing

    else
        Just newName
