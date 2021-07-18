module AST exposing
    ( AggregateType(..)
    , EnumType
    , EnumValue(..)
    , Field(..)
    , NonEnumType(..)
    , PrimitiveType(..)
    , Type(..)
    , UnionMember(..)
    , UserType(..)
    , parseSchemaAST
    )

import Parser exposing ((|.), (|=), Parser, Problem(..), Step(..), Trailing(..))


type UserType
    = Type String NonEnumType
    | Enum String EnumType


type NonEnumType
    = PrimitiveType PrimitiveType
    | AggregateType AggregateType
    | UserTypeName String


type PrimitiveType
    = Int
    | I Int
    | UInt
    | U Int
    | Float Int
    | Bool
    | String
    | Data (Maybe Int)
    | Void


type AggregateType
    = OptionalType NonEnumType
    | ArrayType (Maybe Int) NonEnumType
    | MapType PrimitiveType NonEnumType
    | UnionType (List UnionMember)
    | StructType (List Field)


type Type
    = NonEnumType NonEnumType
    | EnumType EnumType


type alias EnumType =
    List EnumValue


type EnumValue
    = EnumValue String (Maybe Int)


type UnionMember
    = UnionMember NonEnumType (Maybe Int)


type Field
    = Field String NonEnumType


enumType : Parser EnumType
enumType =
    Parser.sequence
        { start = "{"
        , end = "}"
        , separator = ""
        , item = enumValue
        , spaces = whitespace
        , trailing = Optional
        }


enumValue : Parser EnumValue
enumValue =
    Parser.succeed EnumValue
        |= enumValueName
        |. whitespace
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.symbol "="
                |. whitespace
                |= Parser.int
            , Parser.succeed Nothing
            ]


enumValueName : Parser String
enumValueName =
    Parser.getChompedString
        (Parser.chompIf Char.isUpper
            |. Parser.chompWhile (\c -> Char.isUpper c || Char.isDigit c || c == '_')
        )


nonEnumType : Parser NonEnumType
nonEnumType =
    Parser.oneOf
        [ Parser.map PrimitiveType primitiveType
        , Parser.map AggregateType aggregateType
        , Parser.map UserTypeName userTypeName
        ]


primitiveType : Parser PrimitiveType
primitiveType =
    Parser.oneOf
        [ Parser.succeed Int |. Parser.symbol "int"
        , Parser.succeed (I 8) |. Parser.symbol "i8"
        , Parser.succeed (I 16) |. Parser.symbol "i16"
        , Parser.succeed (I 32) |. Parser.symbol "i32"
        , Parser.succeed (I 64) |. Parser.symbol "i64"
        , Parser.succeed UInt |. Parser.symbol "uint"
        , Parser.succeed (U 8) |. Parser.symbol "u8"
        , Parser.succeed (U 16) |. Parser.symbol "u16"
        , Parser.succeed (U 32) |. Parser.symbol "u32"
        , Parser.succeed (U 64) |. Parser.symbol "u64"
        , Parser.succeed (Float 32) |. Parser.symbol "f32"
        , Parser.succeed (Float 64) |. Parser.symbol "f64"
        , Parser.succeed Bool |. Parser.symbol "bool"
        , Parser.succeed String |. Parser.symbol "string"
        , Parser.succeed (Data << Just)
            |. Parser.symbol "data<"
            |= Parser.int
            |. Parser.symbol ">"
        , Parser.succeed (Data Nothing) |. Parser.symbol "data"
        , Parser.succeed Void |. Parser.symbol "void"
        ]
        |. whitespace


aggregateType : Parser AggregateType
aggregateType =
    Parser.oneOf
        [ optionalType
        , arrayType
        , mapType
        , unionType
        , structType
        ]


structType : Parser AggregateType
structType =
    let
        field =
            Parser.succeed Field
                |= Parser.getChompedString
                    (Parser.chompIf Char.isAlpha
                        |. Parser.chompWhile Char.isAlpha
                    )
                |. whitespace
                |. Parser.symbol ":"
                |. whitespace
                |= Parser.lazy (\_ -> nonEnumType)
    in
    Parser.succeed StructType
        |= Parser.sequence
            { start = "{"
            , end = "}"
            , separator = ""
            , trailing = Optional
            , item = field
            , spaces = whitespace
            }



{- struct-type     = "{" [WS] fields [WS] "}"
   fields          = field / (fields WS field)
   field           = 1*ALPHA [WS] ":" [WS] type
-}


unionType : Parser AggregateType
unionType =
    Parser.succeed UnionType
        |= Parser.sequence
            { start = "("
            , end = ")"
            , spaces = whitespace
            , separator = "|"
            , trailing = Forbidden
            , item = unionMember
            }


unionMember : Parser UnionMember
unionMember =
    Parser.succeed UnionMember
        |= Parser.lazy (\_ -> nonEnumType)
        |. whitespace
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.symbol "="
                |. whitespace
                |= Parser.int
            , Parser.succeed Nothing
            ]


mapType : Parser AggregateType
mapType =
    Parser.succeed MapType
        |. Parser.symbol "map["
        |= Parser.lazy (\_ -> primitiveType)
        |. Parser.symbol "]"
        |= Parser.lazy (\_ -> nonEnumType)


arrayType : Parser AggregateType
arrayType =
    Parser.succeed ArrayType
        |. Parser.symbol "["
        |= Parser.oneOf
            [ Parser.succeed Just |= Parser.int
            , Parser.succeed Nothing
            ]
        |. Parser.symbol "]"
        |= Parser.lazy (\_ -> nonEnumType)


optionalType : Parser AggregateType
optionalType =
    Parser.succeed OptionalType
        |. Parser.symbol "optional<"
        |= Parser.lazy (\_ -> nonEnumType)
        |. Parser.symbol ">"


userTypeName : Parser String
userTypeName =
    Parser.getChompedString <|
        Parser.succeed identity
            |. Parser.chompIf Char.isUpper
            |. Parser.chompWhile Char.isAlphaNum


whitespace : Parser ()
whitespace =
    let
        step () =
            Parser.succeed
                (\f t ->
                    if f == t then
                        Done ()

                    else
                        Loop ()
                )
                |= Parser.getOffset
                |. Parser.oneOf
                    [ Parser.lineComment "#"
                    , Parser.spaces
                    ]
                |= Parser.getOffset
    in
    Parser.loop () step


userType :
    List UserType
    -> Parser (Parser.Step (List UserType) (List UserType))
userType acc =
    Parser.oneOf
        [ Parser.map (\t -> Loop <| t :: acc)
            (Parser.succeed Type
                |. Parser.keyword "type"
                |. whitespace
                |= userTypeName
                |. whitespace
                |= nonEnumType
            )
        , Parser.map (\t -> Loop <| t :: acc)
            (Parser.succeed Enum
                |. Parser.keyword "enum"
                |. whitespace
                |= userTypeName
                |. whitespace
                |= enumType
            )
        , Parser.succeed (Done <| List.reverse acc)
            |. Parser.end
        ]
        |. whitespace


mainParser : Parser (List UserType)
mainParser =
    Parser.succeed identity
        |. whitespace
        |= Parser.loop [] userType


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Expecting s ->
            "Expected \"" ++ s ++ "\""

        ExpectingInt ->
            "Expected integer value"

        ExpectingHex ->
            "Expected hex value"

        ExpectingOctal ->
            "Expected octal value"

        ExpectingBinary ->
            "Expected binary value"

        ExpectingFloat ->
            "Expected float value"

        ExpectingNumber ->
            "Expected number value"

        ExpectingVariable ->
            "Expected variable"

        ExpectingSymbol s ->
            "Expected \"" ++ s ++ "\""

        ExpectingKeyword s ->
            "Expected \"" ++ s ++ "\""

        ExpectingEnd ->
            "Expected end"

        UnexpectedChar ->
            "Unexpected char"

        Problem p ->
            p

        BadRepeat ->
            "Bad repeat"


deadEndToString : Parser.DeadEnd -> String
deadEndToString { col, problem, row } =
    " - "
        ++ String.fromInt (row - 2)
        ++ ","
        ++ String.fromInt col
        ++ " "
        ++ problemToString problem


parseSchemaAST : String -> Result String (List UserType)
parseSchemaAST schema =
    schema
        |> Parser.run mainParser
        |> Result.mapError (List.map deadEndToString >> String.join "\n")
