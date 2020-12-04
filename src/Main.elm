module Main exposing (Model)

-- import Json.Decode as D

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import RemoteData exposing (RemoteData(..), WebData, fromResult)


type alias Flags =
    D.Value


type alias Model =
    { flags : Flags
    , text : String
    }


type Msg
    = TextChanged String


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
    ( { flags = flags, text = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        TextChanged newText ->
            ( { model | text = newText }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "QTI Generator"
    , body =
        [ div []
            [ div [] [ text (Debug.toString model) ]
            ]
        ]
    }
