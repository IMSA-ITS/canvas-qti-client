module Main exposing (Model)

-- import Json.Decode as D

import Browser
import Debounce
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (cols, rows)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import RemoteData exposing (RemoteData(..), WebData, fromResult)


type alias Flags =
    D.Value


type alias Model =
    { flags : Flags
    , rawText : String
    , settledText : String
    , debouncer : Debounce.Model String
    }


type Msg
    = TextChanged String
    | DebouncerMsg (Debounce.Msg String)


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
    ( { flags = flags, rawText = "", settledText = "", debouncer = Debounce.init 500 "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        TextChanged newText ->
            { model | rawText = newText } |> updateDebouncer (Debounce.Change newText)

        DebouncerMsg dmsg ->
            updateDebouncer dmsg model

updateDebouncer : Debounce.Msg String -> Model -> ( Model, Cmd Msg )
updateDebouncer dmsg model =
    let
        ( debouncer_, cmd, settledMaybe ) =
            Debounce.update dmsg model.debouncer

        debouncedInput_ =
            settledMaybe |> Maybe.withDefault model.settledText
    in
    ( { model | debouncer = debouncer_, settledText = debouncedInput_ }
    , Cmd.map DebouncerMsg cmd
    )



view : Model -> Browser.Document Msg
view model =
    { title = "QTI Generator"
    , body =
        [ div []
            [ div [] [ text (Debug.toString model) ]
            , Html.textarea [ onInput TextChanged, cols 80, rows 10 ] []
            ]
        ]
    }
