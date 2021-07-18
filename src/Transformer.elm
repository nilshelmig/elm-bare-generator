module Transformer exposing (BareType(..), ElmType(..), EnumInfo, EnumValueInfo, ListInfo, MapInfo, MapKeyType(..), MaybeInfo, RecordField, RecordInfo, TypeAliasInfo, UnionCase, UnionInfo, asBareTypes, hasTypeInformation, resultCollect)

import AST exposing (AggregateType(..), EnumType, EnumValue(..), Field(..), NonEnumType(..), PrimitiveType(..), Type(..), UnionMember(..), UserType(..))
import Set exposing (Set)
import String.Extra as String


type alias RecordField =
    { name : String
    , valueType : ElmType
    }


type alias RecordInfo =
    { fields : List RecordField }


type alias UnionCase =
    { identifier : Int
    , name : String
    , payload : Maybe ElmType
    }


type alias UnionInfo =
    { cases : List UnionCase }


type alias MaybeInfo =
    { maybeType : ElmType }


type alias ListInfo =
    { size : Maybe Int
    , listType : ElmType
    }


type MapKeyType
    = IntKey
    | Int8Key
    | Int16Key
    | Int32Key
    | Int64Key
    | UIntKey
    | UInt8Key
    | UInt16Key
    | UInt32Key
    | UInt64Key
    | Float32Key
    | Float64Key
    | BoolKey
    | StringKey


type alias MapInfo =
    { keyType : MapKeyType
    , valueType : ElmType
    }


type ElmType
    = ElmTypeRecord RecordInfo
    | ElmTypeUnion UnionInfo
    | ElmTypeMaybe MaybeInfo
    | ElmTypeList ListInfo
    | ElmTypeMap MapInfo
    | ElmTypePrimitive PrimitiveType
    | UserType String


type alias TypeAliasInfo =
    { name : String
    , elmType : ElmType
    }


type alias EnumValueInfo =
    { name : String
    , value : Int
    }


type alias EnumInfo =
    { name : String
    , values : List EnumValueInfo
    }


type BareType
    = BareEnum EnumInfo
    | TypeAlias TypeAliasInfo


keyType : PrimitiveType -> Result String MapKeyType
keyType typeinfo =
    case typeinfo of
        Int ->
            Ok IntKey

        I 8 ->
            Ok Int8Key

        I 16 ->
            Ok Int16Key

        I 32 ->
            Ok Int32Key

        I 64 ->
            Ok Int64Key

        I size ->
            Err (String.fromInt size ++ " is an invalid size for unsigned integer")

        UInt ->
            Ok UIntKey

        U 8 ->
            Ok UInt8Key

        U 16 ->
            Ok UInt16Key

        U 32 ->
            Ok UInt32Key

        U 64 ->
            Ok UInt64Key

        U size ->
            Err (String.fromInt size ++ " is an invalid size for unsigned integer")

        Float 32 ->
            Ok Float32Key

        Float 64 ->
            Ok Float64Key

        Float size ->
            Err (String.fromInt size ++ " is an invalid size for float")

        Bool ->
            Ok BoolKey

        String ->
            Ok StringKey

        Data _ ->
            Err "data can't be used as map key"

        Void ->
            Err "void can't be used as map key"


unionCaseName : Int -> NonEnumType -> String
unionCaseName identifier typeinfo =
    case typeinfo of
        PrimitiveType _ ->
            "Primitive" ++ String.fromInt identifier

        AggregateType (MapType _ _) ->
            "Dict" ++ String.fromInt identifier

        AggregateType (OptionalType _) ->
            "Maybe" ++ String.fromInt identifier

        AggregateType (ArrayType _ _) ->
            "List" ++ String.fromInt identifier

        AggregateType (UnionType _) ->
            "Union" ++ String.fromInt identifier

        AggregateType (StructType _) ->
            "Record" ++ String.fromInt identifier

        UserTypeName name ->
            name


elmTypeOf : String -> Set String -> NonEnumType -> Result String ElmType
elmTypeOf typename voidTypes nonEnumType =
    case nonEnumType of
        PrimitiveType primitive ->
            Ok (ElmTypePrimitive primitive)

        UserTypeName name ->
            Ok (UserType name)

        AggregateType (OptionalType option) ->
            elmTypeOf typename voidTypes option
                |> Result.map (\t -> ElmTypeMaybe { maybeType = t })

        AggregateType (MapType key value) ->
            Result.map2
                (\k v -> ElmTypeMap { keyType = k, valueType = v })
                (keyType key)
                (elmTypeOf typename voidTypes value)

        AggregateType (ArrayType size elementType) ->
            elmTypeOf typename voidTypes elementType
                |> Result.map (\t -> ElmTypeList { size = size, listType = t })

        AggregateType (StructType fields) ->
            fields
                |> List.foldl (\(Field name value) result -> result |> Result.andThen (\f -> elmTypeOf typename voidTypes value |> Result.map (\t -> { name = name |> String.decapitalize, valueType = t } :: f)))
                    (Ok [])
                |> Result.map (\f -> ElmTypeRecord { fields = f })

        AggregateType (UnionType members) ->
            members
                |> List.foldl
                    (\(UnionMember valueType identifier) result ->
                        result
                            |> Result.andThen
                                (\( lastIdentifier, cases ) ->
                                    let
                                        currentIdentifier =
                                            identifier |> Maybe.withDefault lastIdentifier

                                        payload =
                                            case valueType of
                                                PrimitiveType Void ->
                                                    Ok Nothing

                                                UserTypeName n ->
                                                    if voidTypes |> Set.member n then
                                                        Ok Nothing

                                                    else
                                                        UserType n |> Just |> Ok

                                                _ ->
                                                    elmTypeOf typename voidTypes valueType |> Result.map Just
                                    in
                                    payload |> Result.map (\t -> ( currentIdentifier + 1, { identifier = currentIdentifier, name = typename ++ unionCaseName currentIdentifier valueType, payload = t } :: cases ))
                                )
                    )
                    (Ok ( 0, [] ))
                |> Result.map Tuple.second
                |> Result.map (\c -> ElmTypeUnion { cases = c })


enumValues : EnumType -> List EnumValueInfo
enumValues enum =
    enum
        |> List.foldl
            (\(EnumValue name value) ( lastValue, values ) ->
                let
                    currentValue =
                        value |> Maybe.withDefault lastValue
                in
                ( currentValue + 1, { name = name, value = currentValue } :: values )
            )
            ( 0, [] )
        |> Tuple.second
        |> List.reverse


asBareType : Set String -> UserType -> Result String BareType
asBareType voidTypes userType =
    case userType of
        Type name nonEnumType ->
            elmTypeOf name voidTypes nonEnumType |> Result.map (\t -> TypeAlias { name = name, elmType = t })

        Enum name enum ->
            BareEnum { name = name, values = enumValues enum }
                |> Ok


userVoidTypes : List UserType -> Set String
userVoidTypes userTypes =
    userTypes
        |> List.filter (hasTypeInformation >> not)
        |> List.map
            (\t ->
                case t of
                    Type name _ ->
                        name

                    Enum name _ ->
                        name
            )
        |> Set.fromList


resultCollect : List (Result err ok) -> Result (List err) (List ok)
resultCollect =
    List.foldl
        (\bareType results ->
            case results of
                Ok types ->
                    bareType
                        |> Result.map (\t -> t :: types)
                        |> Result.mapError (\error -> [ error ])

                Err errors ->
                    case bareType of
                        Ok _ ->
                            Err errors

                        Err error ->
                            Err (error :: errors)
        )
        (Ok [])


asBareTypes : List UserType -> Result (List String) (List BareType)
asBareTypes userTypes =
    userTypes
        |> List.filter hasTypeInformation
        |> List.map (asBareType (userVoidTypes userTypes))
        |> resultCollect


hasTypeInformation : UserType -> Bool
hasTypeInformation userType =
    case userType of
        Type _ (PrimitiveType Void) ->
            False

        _ ->
            True
