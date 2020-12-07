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
    , rawText : String
    , settledText : String
    , debouncer : Debounce.Model String
    , qtiError : WebData String
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
            ( model, requestGenerate model.settledText )

        GotGenerated result ->
            case result of
                Ok bytes ->
                    ( model, downloadZip bytes )

                Err err ->
                    ( model, Cmd.none )


updateDebouncer : Debounce.Msg String -> Model -> ( Model, Cmd Msg )
updateDebouncer dmsg model =
    let
        ( debouncer_, cmd, settledMaybe ) =
            Debounce.update dmsg model.debouncer

        debouncedInput_ =
            settledMaybe |> Maybe.withDefault model.settledText

        requestCmd =
            settledMaybe |> Maybe.map requestQTI |> Maybe.withDefault Cmd.none
    in
    ( { model | debouncer = debouncer_, settledText = debouncedInput_ }
    , Cmd.batch
        [ Cmd.map DebouncerMsg cmd
        , requestCmd
        ]
    )


qtiServerUrl =
    "http://127.0.0.1:5000/validate"


requestQTI : String -> Cmd Msg
requestQTI text =
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
            div [] [ text "loading..." ]

        Success error ->
            viewError error

        Failure e ->
            div [] [ text "failed" ]


generateButton : Model -> Html Msg
generateButton model =
    button [ onClick GenerateClicked ] [ text "Generate" ]
