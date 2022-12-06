module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.ReviewAtDocs
import Elm.CodeGen as CodeGen
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import RecordFieldHelper.GenerateUsed
import Review.Rule as Rule exposing (Rule)
import Simplify
import VariantHelper.GenerateUsed


config : List Rule
config =
    [ Docs.ReviewAtDocs.rule
    , NoConfusingPrefixOperator.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
        -- WIPs go here, don't forget to remove when done
        |> Rule.ignoreErrorsForFiles [ "src/Character.elm", "src/Main.elm" ]
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
        |> Rule.ignoreErrorsForFiles [ "src/Main.elm" ]
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule

    -- Turn this back on when the CI pipeline generates API.elm
    -- , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    , RecordFieldHelper.GenerateUsed.rule
        { generator = erlandsonaV4LensGen
        , generateIn = ( "Fields", [] )
        }
    ]
        |> List.map (Rule.ignoreErrorsForFiles [ "src/API.elm" ])


erlandsonaV4LensGen : RecordFieldHelper.GenerateUsed.FieldLensGenerator
erlandsonaV4LensGen =
    { imports =
        [ CodeGen.importStmt [ "Accessors" ] Nothing <|
            (Just << CodeGen.exposeExplicit)
                [ CodeGen.funExpose "lens"
                , CodeGen.typeOrAliasExpose "Lens"
                ]
        ]
    , declaration =
        \{ fieldName } ->
            { documentation = Nothing
            , name = fieldName
            , annotation =
                (Just << CodeGen.typed "Lens") <|
                    CodeGen.typeVar "ls"
                        :: CodeGen.extRecordAnn "r" [ ( fieldName, CodeGen.typeVar "a" ) ]
                        :: List.map CodeGen.typeVar [ "a", "x", "y" ]
            , implementation =
                CodeGen.construct "lens"
                    [ CodeGen.string (String.cons '.' fieldName)
                    , (RecordFieldHelper.GenerateUsed.functionsForField fieldName).access
                    , CodeGen.lambda (List.map CodeGen.varPattern [ "rec", "val" ]) <|
                        CodeGen.update "rec" [ ( fieldName, CodeGen.val "val" ) ]
                    ]
            }
    }
