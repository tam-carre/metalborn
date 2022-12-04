module API.Gender exposing (GenderData, fromStr, info)

import API
import Dict


type alias GenderData =
    { letter : String
    , str : String
    }


fromStr : String -> Maybe API.Gender
fromStr str =
    (Dict.get str << Dict.fromList)
        [ ( "Male", API.Male )
        , ( "Female", API.Female )
        , ( "Other", API.Other )
        ]


info : API.Gender -> GenderData
info gender =
    case gender of
        API.Male ->
            { letter = "M", str = "Male" }

        API.Female ->
            { letter = "F", str = "Female" }

        API.Other ->
            { letter = "O", str = "Other" }
