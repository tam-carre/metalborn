module Gender exposing (fromStr, toLetter)

import API
import Dict


fromStr : String -> Maybe API.Gender
fromStr str =
    (Dict.get str << Dict.fromList)
        [ ( "Male", API.Male )
        , ( "Female", API.Female )
        , ( "Other", API.Other )
        ]


toLetter : API.Gender -> String
toLetter gender =
    case gender of
        API.Male ->
            "M"

        API.Female ->
            "F"

        API.Other ->
            "O"
