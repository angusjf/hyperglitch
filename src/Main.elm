port module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)
import List
import Maybe
import Glitches exposing (asdfPixelSort, brightness)
import Image exposing (Image, Pixel, imageToJson, jsonToImage)
import Json.Encode as E

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { image : Image }

type Msg = NewImage E.Value | SendToJS

init : (E.Value) -> (Model, Cmd Msg)
init json = ({ image = jsonToImage json}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case Debug.log "[elm]" msg of
        NewImage newImage ->
            ({ model | image = jsonToImage newImage }, Cmd.none)
        SendToJS ->
            let
                newImage = asdfPixelSort (\p -> brightness p > 127) model.image
            in
                (model, toJs (imageToJson newImage))

subscriptions : Model -> Sub Msg
subscriptions model = toElm NewImage

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendToJS ] [ text "glitch!" ] ]

port toElm : (E.Value -> msg) -> Sub msg
port toJs : E.Value -> Cmd msg
