module Main exposing (Model)

-- import Json.Decode as D

import Browser
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Debounce
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (class, cols, rows, style, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Http exposing (Error(..), Response(..))
import Json.Decode as D
import RemoteData exposing (RemoteData(..), WebData, fromResult)
import Task


type alias Flags =
    D.Value


type alias Model =
    { host : String
    , rawText : String -- input from user
    , settledText : String -- debounced/settled input
    , debouncer : Debounce.Model String -- state of debounce
    , qtiError : WebData String -- error from QTI generation
    , dragHover : Bool
    }


type Msg
    = OnInput Input
    | OnClick Button
    | OnResponse Response
    | OnDrag DragEvent
    | FileSelected File
    | FileLoaded String
    | OnDebounceEvent (Debounce.Msg String)


type Input
    = Text String


type Button
    = Generate
    | Open


type Response
    = Validation (WebData String)
    | Generation (Result Http.Error Bytes)


type DragEvent
    = Enter
    | Leave
    | Drop File (List File)


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
    ( { host = D.decodeValue flagsDecoder flags |> Result.withDefault "http://localhost:8081"
      , rawText = ""
      , settledText = ""
      , debouncer = Debounce.init 500 ""
      , qtiError = NotAsked
      , dragHover = False
      }
    , Cmd.none
    )


flagsDecoder : D.Decoder String
flagsDecoder =
    D.field "qtihost" D.string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --case Debug.log "msg" msg of
    case msg of
        OnInput (Text newText) ->
            { model | rawText = newText } |> updateDebouncer (Debounce.Change newText)

        OnClick Generate ->
            ( { model | qtiError = Loading }, requestGenerate model.host model.settledText )

        OnClick Open ->
            ( model, File.Select.file [ "text/plain", "text/markdown" ] FileSelected )

        OnDrag dragEvent ->
            case dragEvent of
                Enter ->
                    ( { model | dragHover = True }, Cmd.none )

                Leave ->
                    ( { model | dragHover = False }, Cmd.none )

                Drop file files ->
                    -- we only handle first file dropped (TODO?)
                    ( { model | dragHover = False }, Task.perform FileLoaded (File.toString file) )

        OnDebounceEvent dmsg ->
            updateDebouncer dmsg model

        OnResponse (Validation error) ->
            ( { model | qtiError = error }, Cmd.none )

        OnResponse (Generation result) ->
            case result of
                Ok bytes ->
                    ( model, downloadZip bytes )

                Err err ->
                    ( { model | qtiError = Success "network error" }, Cmd.none )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded contents ->
            ( { model | rawText = contents, settledText = contents }, Cmd.none )


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
            , Cmd.batch
                [ requestQTI model.host text
                , Cmd.map OnDebounceEvent cmd
                ]
            )

        Nothing ->
            ( { model | debouncer = debouncer_ }, Cmd.map OnDebounceEvent cmd )


requestQTI : String -> String -> Cmd Msg
requestQTI host text =
    -- request validation of the input text
    Http.request
        { method = "POST"
        , url = host ++ "/validate"
        , body = Http.stringBody "text/plain" text
        , expect = Http.expectJson (fromResult >> (OnResponse << Validation)) decodeQTIResponse
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }


requestGenerate : String -> String -> Cmd Msg
requestGenerate host text =
    -- Request to generate and download zip archive of QTI content
    Http.request
        { method = "POST"
        , url = host ++ "/generate"
        , body = Http.stringBody "text/plain" text
        , expect = Http.expectBytesResponse (OnResponse << Generation) handleGenerateResponse
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }


decodeQTIResponse : D.Decoder String
decodeQTIResponse =
    D.field "error" D.string


handleGenerateResponse : Http.Response Bytes -> Result Http.Error Bytes
handleGenerateResponse response =
    {- Because we want the raw bytes from the response, we seem to need this custom
       handler as there does not seem to be a Bytes.Decoder that is just the identity
       function.
    -}
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
            [ Html.textarea [ onInput (OnInput << Text), cols 80, rows 10, value model.rawText ] []
            , viewResponse model.qtiError
            , generateButton model
            , openButton
            , dropArea model.dragHover
            --, div [] [ text (Debug.toString model) ]
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
            div [] [ text "network failure" ]


generateButton : Model -> Html Msg
generateButton { qtiError } =
    -- Display the Generate button only when there are no validation errors
    case qtiError of
        Success "" ->
            button [ onClick <| OnClick Generate ] [ text "Generate" ]

        _ ->
            div [] []


openButton : Html Msg
openButton =
    button [ onClick <| OnClick Open ] [ text "Open file" ]


dropArea : Bool -> Html Msg
dropArea hovering =
    -- See example at https://elm-lang.org/examples/drag-and-drop
    div
        [ class "dropArea"
        , style "border"
            (if hovering then
                "6px dashed purple"

             else
                "6px dashed #ccc"
            )
        , hijackOn "dragenter" (D.succeed (OnDrag Enter))
        , hijackOn "dragover" (D.succeed (OnDrag Enter))
        , hijackOn "dragleave" (D.succeed (OnDrag Leave))
        , hijackOn "drop" dropDecoder
        ]
        [ text "drop file here"
        ]


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    let
        hijack msg =
            ( msg, True )
    in
    preventDefaultOn event (D.map hijack decoder)


dropDecoder : D.Decoder Msg
dropDecoder =
    let
        tag file files =
            OnDrag (Drop file files)
    in
    D.at [ "dataTransfer", "files" ] (D.oneOrMore tag File.decoder)
