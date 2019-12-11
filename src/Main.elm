port module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, img, li, ul)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)
import List
import List.Extra
import Maybe
import Glitches exposing (Filter, filters)
import Image exposing (Image, Pixel, imageToJson, jsonToImage)
import Json.Encode as E
import Random
import Cmd.Extra

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { image   : Image
    , filters : List Filter }

type Msg = NewImage E.Value
         | SendToJs
         | AddFilter Filter
         | RemoveFilter Int

init : (E.Value) -> (Model, Cmd Msg)
init json = ({ image = jsonToImage json, filters = []}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case Debug.log "[elm]" msg of
        NewImage newImage ->
            ({ model | image = jsonToImage newImage }, Cmd.none)
        SendToJs ->
            let
                fns = List.map .glitch model.filters
                (newImage, _) = applyAll fns (model.image, Random.initialSeed 4)
            in
                (model, toJs (imageToJson newImage))
        AddFilter filter ->
            ({ model | filters = filter::model.filters }, Cmd.Extra.perform SendToJs)
        RemoveFilter n ->
            ({ model | filters = List.Extra.removeAt n model.filters }, Cmd.Extra.perform SendToJs)

applyAll : List (a -> a) -> a -> a
applyAll functions input = case functions of
    f::fs -> applyAll fs (f input)
    []    -> input

subscriptions : Model -> Sub Msg
subscriptions model = toElm NewImage

view : Model -> Html Msg
view model =
    div []
        [ li [] <| List.indexedMap viewFilter model.filters
        , div [] <| List.map viewAddFilterButton Glitches.filters
        --button [ onClick SendToJs ] [ text "glitch!" ]
        ]

viewFilter : Int -> Filter -> Html Msg
viewFilter n filter =
    ul []
        [ text filter.desc
        , button [ onClick (RemoveFilter n) ] [ text "x" ]
        ]

viewAddFilterButton : Filter -> Html Msg
viewAddFilterButton filter =
    let
        buttonText = "Add '" ++ filter.desc ++ "'!'"
    in
        button [ onClick (AddFilter filter) ] [ text buttonText ]

port toElm : (E.Value -> msg) -> Sub msg
port toJs : E.Value -> Cmd msg
