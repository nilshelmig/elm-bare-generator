port module Main exposing (main)

import AST exposing (UserType, parseSchemaAST)
import Generator exposing (generate)


port notifyStepParsing : () -> Cmd msg


port notifyStepTransforming : () -> Cmd msg


port notifyStepWriteFile : () -> Cmd msg


port notified : ({} -> msg) -> Sub msg


port writeFile : String -> Cmd msg


port writeFileDone : ({} -> msg) -> Sub msg


port exitWithError : String -> Cmd msg


port exit : () -> Cmd msg


type Step
    = Parsing String
    | Transforming (List UserType)
    | WriteFile String
    | WaitForFile


type alias Model =
    { step : Step
    , moduleName : String
    }


type alias Flags =
    { schema : String
    , moduleName : String
    }


type Msg
    = StartParsing String
    | StartTransforming (List UserType)
    | StartWriteFile String
    | WriteFileDone


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.step of
        Parsing schema ->
            notified (always (StartParsing schema))

        Transforming ast ->
            notified (always (StartTransforming ast))

        WriteFile content ->
            notified (always (StartWriteFile content))

        WaitForFile ->
            writeFileDone (always WriteFileDone)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartParsing schema ->
            case parseSchemaAST schema of
                Ok ast ->
                    ( { model | step = Transforming ast }, notifyStepTransforming () )

                Err error ->
                    ( model, exitWithError error )

        StartTransforming ast ->
            case generate model.moduleName ast of
                Ok file ->
                    ( { model | step = WriteFile file }, notifyStepWriteFile () )

                Err errors ->
                    ( model, exitWithError (String.join "\n" errors) )

        StartWriteFile content ->
            ( { model | step = WaitForFile }, writeFile content )

        WriteFileDone ->
            ( model, exit () )


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { step = Parsing flags.schema, moduleName = flags.moduleName }
    , notifyStepParsing ()
    )
