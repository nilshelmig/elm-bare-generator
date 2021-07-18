module Generator exposing (generate)

import AST exposing (PrimitiveType(..), UserType)
import Elm.CodeGen as Gen exposing (Declaration, Expression, Import, Pattern, TopLevelExpose, TypeAnnotation)
import Elm.Pretty
import String.Extra
import Transformer exposing (BareType(..), ElmType(..), EnumInfo, MapKeyType(..), RecordInfo, UnionCase, UnionInfo, asBareTypes, resultCollect)


enumTypeDecalration : EnumInfo -> Declaration
enumTypeDecalration enum =
    Gen.customTypeDecl
        (Gen.emptyDocComment |> Gen.markdown ("Convert to message with `" ++ encoderName enum.name ++ "`\n  Create from Message with " ++ decoderName enum.name) |> Just)
        enum.name
        []
        (enum.values |> List.map (\v -> ( v.name, [] )))


unionPayloadTypeAnnotation : Maybe ElmType -> Result String (List TypeAnnotation)
unionPayloadTypeAnnotation payload =
    case payload of
        Just p ->
            typeAnnotation p |> Result.map List.singleton

        Nothing ->
            Ok []


unionTypeDeclaration : String -> UnionInfo -> Result String Declaration
unionTypeDeclaration name union =
    union.cases
        |> List.map (\c -> unionPayloadTypeAnnotation c.payload |> Result.map (\a -> ( c.name, a )))
        |> resultCollect
        |> Result.map
            (\a ->
                Gen.customTypeDecl
                    (Gen.emptyDocComment |> Gen.markdown ("Convert to message with `" ++ encoderName name ++ "`\n  Create from Message with " ++ decoderName name) |> Just)
                    name
                    []
                    a
            )
        |> Result.mapError String.concat


primitiveTypeToAnnotation : PrimitiveType -> TypeAnnotation
primitiveTypeToAnnotation ptype =
    case ptype of
        Int ->
            Gen.intAnn

        I _ ->
            Gen.intAnn

        UInt ->
            Gen.intAnn

        U _ ->
            Gen.intAnn

        Float _ ->
            Gen.floatAnn

        Bool ->
            Gen.boolAnn

        String ->
            Gen.stringAnn

        Data _ ->
            Gen.typed "Bytes" []

        Void ->
            Gen.unitAnn


mapKeyAnnotation : MapKeyType -> TypeAnnotation
mapKeyAnnotation key =
    case key of
        IntKey ->
            Gen.intAnn

        Int8Key ->
            Gen.intAnn

        Int16Key ->
            Gen.intAnn

        Int32Key ->
            Gen.intAnn

        Int64Key ->
            Gen.intAnn

        UIntKey ->
            Gen.intAnn

        UInt8Key ->
            Gen.intAnn

        UInt16Key ->
            Gen.intAnn

        UInt32Key ->
            Gen.intAnn

        UInt64Key ->
            Gen.intAnn

        Float32Key ->
            Gen.floatAnn

        Float64Key ->
            Gen.floatAnn

        BoolKey ->
            Gen.boolAnn

        StringKey ->
            Gen.stringAnn


typeAnnotation : ElmType -> Result String TypeAnnotation
typeAnnotation typeinfo =
    case typeinfo of
        ElmTypePrimitive primitive ->
            Ok (primitiveTypeToAnnotation primitive)

        ElmTypeList list ->
            typeAnnotation list.listType
                |> Result.map Gen.listAnn

        ElmTypeMaybe maybe ->
            typeAnnotation maybe.maybeType
                |> Result.map Gen.maybeAnn

        ElmTypeMap map ->
            typeAnnotation map.valueType
                |> Result.map (\a -> Gen.dictAnn (mapKeyAnnotation map.keyType) a)

        ElmTypeRecord record ->
            record.fields
                |> List.map (\f -> typeAnnotation f.valueType |> Result.map (\a -> ( f.name, a )))
                |> List.foldl (\result annotations -> annotations |> Result.andThen (\a -> result |> Result.map (\r -> r :: a))) (Ok [])
                |> Result.map Gen.recordAnn

        ElmTypeUnion _ ->
            Err "Anonymous unions are not implemented"

        UserType name ->
            Ok (Gen.typeVar name)


toTypeDeclaration : BareType -> Result String Declaration
toTypeDeclaration userType =
    case userType of
        BareEnum enum ->
            Ok (enumTypeDecalration enum)

        TypeAlias aliasInfo ->
            case aliasInfo.elmType of
                ElmTypeUnion union ->
                    unionTypeDeclaration aliasInfo.name union

                elmType ->
                    typeAnnotation elmType
                        |> Result.map
                            (\a ->
                                Gen.aliasDecl
                                    (Gen.emptyDocComment |> Gen.markdown ("Convert to message with `" ++ encoderName aliasInfo.name ++ "`\n  Create from Message with " ++ decoderName aliasInfo.name) |> Just)
                                    aliasInfo.name
                                    []
                                    a
                            )


typeDeclarations : List BareType -> Result (List String) (List Declaration)
typeDeclarations =
    List.map toTypeDeclaration
        >> resultCollect


enumCodec : EnumInfo -> Expression
enumCodec enum =
    enum.values
        |> List.map (\v -> Gen.tuple [ Gen.val v.name, Gen.int v.value ])
        |> Gen.list
        |> List.singleton
        |> Gen.fqConstruct [ "Codec" ] "enumWithValues"


primitiveCodec : PrimitiveType -> Expression
primitiveCodec primitive =
    case primitive of
        Int ->
            Gen.fqFun [ "Codec" ] "int"

        -- i64 is not supported yet by elm-bare
        I 64 ->
            Gen.fqFun [ "Codec" ] "int"

        I size ->
            Gen.fqFun [ "Codec" ] ("i" ++ String.fromInt size)

        UInt ->
            Gen.fqFun [ "Codec" ] "uint"

        U size ->
            Gen.fqFun [ "Codec" ] ("u" ++ String.fromInt size)

        Float size ->
            Gen.fqFun [ "Codec" ] ("f" ++ String.fromInt size)

        Bool ->
            Gen.fqFun [ "Codec" ] "bool"

        String ->
            Gen.fqFun [ "Codec" ] "string"

        Data length ->
            case length of
                Just l ->
                    Gen.fqConstruct [ "Codec" ] "dataWithLength" [ Gen.int l ]

                Nothing ->
                    Gen.fqFun [ "Codec" ] "data"

        Void ->
            Gen.fqFun [ "Codec" ] "void"


mapKeyCodecName : MapKeyType -> String
mapKeyCodecName key =
    case key of
        IntKey ->
            "int"

        Int8Key ->
            "i8"

        Int16Key ->
            "i16"

        Int32Key ->
            "i32"

        -- i64 is not yet supported by elm-bare
        Int64Key ->
            "int"

        UIntKey ->
            "uint"

        UInt8Key ->
            "u8"

        UInt16Key ->
            "u16"

        UInt32Key ->
            "u32"

        UInt64Key ->
            "u64"

        Float32Key ->
            "f32"

        Float64Key ->
            "f64"

        BoolKey ->
            "bool"

        StringKey ->
            "string"


recordConstruct : RecordInfo -> Expression
recordConstruct record =
    Gen.lambda
        (record.fields |> List.map (\f -> Gen.varPattern f.name))
        (record.fields |> List.map (\f -> ( f.name, Gen.val f.name )) |> Gen.record)


recordFieldsCoded : RecordInfo -> Expression -> Expression
recordFieldsCoded record expression =
    [ record.fields
        |> List.reverse
        |> List.map (\f -> Gen.fqConstruct [ "Codec" ] "field" [ Gen.accessFun ("." ++ f.name), elmTypeCodec Nothing f.valueType |> Gen.parens ])
    , [ Gen.fqFun [ "Codec" ] "buildStruct" ]
    ]
        |> List.concat
        |> Gen.pipe expression


anonymousRecordCodec : RecordInfo -> Expression
anonymousRecordCodec record =
    Gen.fqConstruct [ "Codec" ] "struct" [ recordConstruct record ]
        |> recordFieldsCoded record


recordCodec : String -> RecordInfo -> Expression
recordCodec name record =
    Gen.fqConstruct [ "Codec" ] "struct" [ Gen.val name ]
        |> recordFieldsCoded record


unionCaseCodec : UnionCase -> Expression
unionCaseCodec unionCase =
    case unionCase.payload of
        Just payload ->
            [ Gen.int unionCase.identifier
            , Gen.val unionCase.name
            , Gen.parens (elmTypeCodec Nothing payload)
            ]
                |> Gen.fqConstruct [ "Codec" ] "variant1"

        Nothing ->
            [ Gen.int unionCase.identifier
            , Gen.val unionCase.name
            ]
                |> Gen.fqConstruct [ "Codec" ] "variant0"


unionMatchCase : UnionCase -> ( Pattern, Expression )
unionMatchCase unionCase =
    case unionCase.payload of
        Just _ ->
            ( Gen.namedPattern unionCase.name [ Gen.varPattern "p" ], Gen.fqConstruct [] (unionCase.name |> String.Extra.decapitalize) [ Gen.val "p" ] )

        Nothing ->
            ( Gen.namedPattern unionCase.name [], Gen.val (unionCase.name |> String.Extra.decapitalize) )


unionMatch : UnionInfo -> Expression
unionMatch union =
    Gen.lambda
        ([ union.cases
            |> List.reverse
            |> List.map (\c -> c.name |> String.Extra.decapitalize |> Gen.varPattern)
         , [ Gen.varPattern "value" ]
         ]
            |> List.concat
        )
        (Gen.caseExpr
            (Gen.val "value")
            (union.cases
                |> List.reverse
                |> List.map unionMatchCase
            )
        )


unionCodec : UnionInfo -> Expression
unionCodec union =
    [ union.cases
        |> List.reverse
        |> List.map unionCaseCodec
    , [ Gen.fqFun [ "Codec" ] "buildTaggedUnion" ]
    ]
        |> List.concat
        |> Gen.pipe (Gen.fqConstruct [ "Codec" ] "taggedUnion" [ unionMatch union ])


elmTypeCodec : Maybe String -> ElmType -> Expression
elmTypeCodec aliasType elmType =
    case elmType of
        UserType name ->
            Gen.val (toCodecName name)

        ElmTypePrimitive primitive ->
            primitiveCodec primitive

        ElmTypeMaybe maybe ->
            Gen.fqConstruct [ "Codec" ] "optional" [ elmTypeCodec Nothing maybe.maybeType |> Gen.parens ]

        ElmTypeList list ->
            case list.size of
                Just size ->
                    Gen.fqConstruct [ "Codec" ] "listWithLength" [ Gen.int size, elmTypeCodec Nothing list.listType |> Gen.parens ]

                Nothing ->
                    Gen.fqConstruct [ "Codec" ] "list" [ elmTypeCodec Nothing list.listType |> Gen.parens ]

        ElmTypeMap map ->
            Gen.fqConstruct [ "Codec" ] "dict" [ Gen.fqFun [ "Codec" ] (mapKeyCodecName map.keyType), elmTypeCodec Nothing map.valueType |> Gen.parens ]

        ElmTypeRecord record ->
            case aliasType of
                Just t ->
                    recordCodec t record

                Nothing ->
                    anonymousRecordCodec record

        ElmTypeUnion union ->
            unionCodec union


toCodec : BareType -> Result String Declaration
toCodec userType =
    case userType of
        BareEnum enum ->
            Gen.funDecl
                Nothing
                (Just (Gen.typed "Codec" [ Gen.typed enum.name [] ]))
                (toCodecName enum.name)
                []
                (enumCodec enum)
                |> Ok

        TypeAlias aliasInfo ->
            Gen.funDecl
                Nothing
                (Just (Gen.typed "Codec" [ Gen.typed aliasInfo.name [] ]))
                (toCodecName aliasInfo.name)
                []
                (elmTypeCodec (Just aliasInfo.name) aliasInfo.elmType)
                |> Ok


codecs : List BareType -> Result (List String) (List Declaration)
codecs =
    List.map toCodec
        >> resultCollect


encoderName : String -> String
encoderName name =
    "from" ++ name


decoderName : String -> String
decoderName name =
    "to" ++ name


toConverter : BareType -> List Declaration
toConverter userType =
    let
        name =
            case userType of
                BareEnum enum ->
                    enum.name

                TypeAlias aliasInfo ->
                    aliasInfo.name
    in
    [ Gen.funDecl
        Nothing
        (Just (Gen.typed name [] |> Gen.maybeAnn |> Gen.funAnn (Gen.typed "Bytes" [])))
        (decoderName name)
        []
        (Gen.fqConstruct [ "Codec" ] "decodeValue" [ Gen.val (toCodecName name) ])
    , Gen.funDecl
        Nothing
        (Just (Gen.typed "Bytes" [] |> Gen.funAnn (Gen.typed name [])))
        (encoderName name)
        []
        (Gen.fqConstruct [ "Codec" ] "encodeToValue" [ Gen.val (toCodecName name) ])
    ]


converters : List BareType -> List Declaration
converters =
    List.map toConverter >> List.concat


imports : List Import
imports =
    [ Gen.importStmt [ "Codec", "Bare" ]
        (Just [ "Codec" ])
        (Just (Gen.exposeExplicit [ Gen.closedTypeExpose "Codec", Gen.closedTypeExpose "Bytes" ]))
    , Gen.importStmt [ "Dict" ]
        Nothing
        (Just (Gen.exposeExplicit [ Gen.closedTypeExpose "Dict" ]))
    ]


toCodecName : String -> String
toCodecName typeName =
    "codecOf" ++ typeName


isAlias : BareType -> Bool
isAlias userType =
    case userType of
        TypeAlias info ->
            case info.elmType of
                ElmTypeUnion _ ->
                    False

                _ ->
                    True

        BareEnum _ ->
            False


toExpose : BareType -> List TopLevelExpose
toExpose userType =
    let
        name =
            case userType of
                BareEnum enum ->
                    enum.name

                TypeAlias info ->
                    info.name
    in
    [ if isAlias userType then
        Gen.closedTypeExpose name

      else
        Gen.openTypeExpose name
    , Gen.funExpose (encoderName name)
    , Gen.funExpose (decoderName name)
    ]


declarations : List BareType -> Result (List String) ( List Declaration, List TopLevelExpose )
declarations types =
    Result.map2
        (++)
        (typeDeclarations types)
        (codecs types)
        |> Result.map (\d -> d ++ converters types)
        |> Result.map (\d -> ( d, List.concatMap toExpose types ))


generate : String -> List UserType -> Result (List String) String
generate moduleName userTypes =
    userTypes
        |> asBareTypes
        |> Result.andThen declarations
        |> Result.map
            (\( decls, exposes ) ->
                Gen.file
                    (Gen.normalModule (String.split "." moduleName) exposes)
                    imports
                    decls
                    Nothing
            )
        |> Result.map (Elm.Pretty.pretty 120)
