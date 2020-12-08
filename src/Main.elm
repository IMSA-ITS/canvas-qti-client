module Main exposing (Model)

-- import Json.Decode as D

import Browser
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Debounce
import File.Download
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, cols, rows)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..), Response(..))
import Json.Decode as D
import RemoteData exposing (RemoteData(..), WebData, fromResult)


type alias Flags =
    D.Value


type alias Model =
    { flags : Flags
    , rawText : String -- input from user
    , settledText : String -- debounced/settled input
    , debouncer : Debounce.Model String -- state of debounce
    , qtiError : WebData String -- error from QTI generation
    }


type Msg
    = TextChanged String
    | DebouncerMsg (Debounce.Msg String)
    | GotQTI (WebData String)
    | GotGenerated (Result Http.Error Bytes)
    | GenerateClicked


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , rawText = ""
      , settledText = ""
      , debouncer = Debounce.init 1000 ""
      , qtiError = NotAsked
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        TextChanged newText ->
            { model | rawText = newText } |> updateDebouncer (Debounce.Change newText)

        DebouncerMsg dmsg ->
            updateDebouncer dmsg model

        GotQTI error ->
            ( { model | qtiError = error }, Cmd.none )

        GenerateClicked ->
            ( { model | qtiError = Loading }, requestGenerate model.settledText )

        GotGenerated result ->
            case result of
                Ok bytes ->
                    ( model, downloadZip bytes )

                Err err ->
                    ( model, Cmd.none )


updateDebouncer : Debounce.Msg String -> Model -> ( Model, Cmd Msg )
updateDebouncer dmsg model =
    -- When the input text settles, return command to request QTI generation
    let
        ( debouncer_, cmd, settledMaybe ) =
            Debounce.update dmsg model.debouncer
    in
    case settledMaybe of
        Just text ->
            ( { model | debouncer = debouncer_, settledText = text, qtiError = Loading }
            , Cmd.batch [ requestQTI text, Cmd.map DebouncerMsg cmd ]
            )

        Nothing ->
            ( { model | debouncer = debouncer_ }, Cmd.map DebouncerMsg cmd )


qtiServerUrl =
    "http://127.0.0.1:5000/validate"


requestQTI : String -> Cmd Msg
requestQTI text =
    -- request validation of the input text
    Http.request
        { method = "POST"
        , url = qtiServerUrl
        , body = Http.stringBody "text/plain" text
        , expect = Http.expectJson (fromResult >> GotQTI) decodeQTIResponse
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }


requestGenerate : String -> Cmd Msg
requestGenerate text =
    -- Request to generate and download zip archive of QTI content
    Http.request
        { method = "POST"
        , url = qtiServerUrl ++ "?generate"
        , body = Http.stringBody "text/plain" text
        , expect = Http.expectBytesResponse GotGenerated handleGenerateResponse
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }


decodeQTIResponse : D.Decoder String
decodeQTIResponse =
    D.field "error" D.string



{- Because we want the raw bytes from the response, we seem to need this custom
   handler as there does not seem to be a Bytes.Decoder that is just the identity
   function.
-}


handleGenerateResponse : Http.Response Bytes -> Result Http.Error Bytes
handleGenerateResponse response =
    case response of
        BadUrl_ url ->
            Err (BadUrl url)

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadStatus_ metadata _ ->
            Err (BadStatus metadata.statusCode)

        GoodStatus_ _ body ->
            Ok body


downloadZip : Bytes -> Cmd msg
downloadZip bytes =
    File.Download.bytes "qti.zip" "application/zip" bytes


view : Model -> Browser.Document Msg
view model =
    { title = "QTI Generator"
    , body =
        [ div []
            [ Html.textarea [ onInput TextChanged, cols 80, rows 10 ] []
            , viewResponse model.qtiError
            , generateButton model

            -- , div [] [ text (Debug.toString model) ]
            ]
        ]
    }


viewResponse : WebData String -> Html msg
viewResponse response =
    let
        viewError error =
            case error of
                "" ->
                    div [] [ text "OK" ]

                _ ->
                    div [ class "error" ] [ text error ]
    in
    case response of
        NotAsked ->
            div [] [ text "not asked" ]

        Loading ->
            div [] [ text "validating..." ]

        Success error ->
            viewError error

        Failure e ->
            div [] [ text "failed" ]


generateButton : Model -> Html Msg
generateButton { qtiError } =
    -- Display the Generate button only when there are no validation errors
    case qtiError of
        Success "" ->
            button [ onClick GenerateClicked ] [ text "Generate" ]

        _ ->
            div [] []
