port module Main exposing (..)

import Browser
import List
import List.Extra
import Maybe
import Filters exposing (Filter, ArgKey, ArgValue, ArgDict)
import Image exposing (Image, Pixel, imageToJson, jsonToImage)
import Json.Decode as D
import Random
import Tuple
import Cmd.Extra
import Dict exposing (Dict)
import Html
import Html.Attributes
import Element as Element exposing (Element, el, row, column, text, Color, rgb, spacing, padding, rgb255)
import Element.Input as Input
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border
import File.Select as Select
import File exposing (File)
import Task

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { image          : Maybe Image
    , imageBase64    : Maybe String
    , currentFilters : List FilterAndArgs
    }

type alias FilterAndArgs =
    { args : Filters.ArgDict
    , filter : Filter
    }

type Msg = NewImage D.Value
         | SendToJs
         | AddFilter FilterAndArgs
         | RemoveFilter Int
         | ArgSliderChanged Int String Int
         | UploadImage
         | ImageLoaded File
         | ImageConverted String

init : () -> (Model, Cmd Msg)
init _ =
    let
        model =
            { image = Nothing --jsonToImage json
            , imageBase64 = Nothing
            , currentFilters = []
            }
    in
        (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case Debug.log "[elm]" msg of
        NewImage newImage ->
            case Debug.log "+++++++++" <| jsonToImage newImage of
                Ok image ->
                    ({ model | image = Just image }, Cmd.Extra.perform SendToJs)
                Err error ->
                    ({ model | image = Nothing }, Cmd.Extra.perform SendToJs)
        SendToJs ->
            let
                applyArgs = \f -> f.filter.glitch f.args
                fns = List.filterMap applyArgs model.currentFilters
                newImage =
                    case model.image of
                        Just image -> Just <| Tuple.first <| applyAll fns (image, Random.initialSeed 4)
                        Nothing -> Nothing
            in
                case Debug.log ":: " newImage of
                    Just image -> (model, toJs (imageToJson image))
                    Nothing -> (model, Cmd.none)
        AddFilter filter ->
            ({ model | currentFilters = model.currentFilters ++ [filter] }, Cmd.Extra.perform SendToJs)
        RemoveFilter n ->
            ({ model | currentFilters = List.Extra.removeAt n model.currentFilters }, Cmd.Extra.perform SendToJs)
        ArgSliderChanged n key newValue ->
            let
                updateArg : Maybe ArgValue -> Maybe ArgValue
                updateArg arg =
                    case arg of
                        Just v -> Just { v | value = newValue }
                        Nothing -> Nothing
                updateFilter : FilterAndArgs -> FilterAndArgs
                updateFilter filter = { filter | args = Dict.update key updateArg filter.args }
                newCurrentFilters : List FilterAndArgs
                newCurrentFilters = List.Extra.updateAt n updateFilter model.currentFilters
            in
                ({ model | currentFilters = newCurrentFilters }, Cmd.Extra.perform SendToJs)
        UploadImage ->
            (model, Select.file ["image/*"] ImageLoaded)
        ImageLoaded file ->
            (model, Task.perform ImageConverted (File.toUrl file))
        ImageConverted base64String ->
            ( { model | imageBase64 = Just base64String }
            , base64ToJs base64String
            )

applyAll : List (a -> a) -> a -> a
applyAll functions input = case functions of
    f::fs -> applyAll fs (f input)
    []    -> input

subscriptions : Model -> Sub Msg
subscriptions model = toElm NewImage

view : Model -> Html.Html Msg
view model =
    Element.layout
        [ Element.padding 16
        , Element.spacing 16
        ] <|
        column [] <|
            [ row []
                [ Element.image
                    [ id "uploadedImage" ]
                    { src = Maybe.withDefault "" model.imageBase64
                    , description = ""
                    }
                , Element.html canvas
                , column [] <|
                    List.indexedMap viewFilterAndArgs model.currentFilters
                ]
            , button UploadImage "upload an image!"
            , column [ Element.alignTop ] <|
                List.map viewFilters Filters.filters
            ]

button msg str = 
  Input.button
    [ Background.color blue
    , Element.paddingXY 8 4
    , Font.color white
    , Font.bold
    , Border.rounded 4
    ]
    { onPress = Just msg
    , label = text str
    }
canvas : Html.Html Msg
canvas = Html.canvas [ Html.Attributes.id "hidden-canvas" ] []

id : String -> Element.Attribute msg
id = Element.htmlAttribute << Html.Attributes.id

-------------
-- FILTERS --
-------------

blue : Color
blue = rgb255 14 255 223

lightBlue : Color
lightBlue = rgb 0.3 0.8 0.9

red : Color
red = rgb 0.8 0.3 0.1

lightGrey : Color
lightGrey = rgb 0.9 0.9 0.9

grey : Color
grey = rgb 0.5 0.5 0.5

white = rgb 1 1 1

viewFilters : (String, List Filter) -> Element Msg
viewFilters (groupName, filters) =
    column
        []
        [ (text groupName)
        , column [] <| List.map viewAddFilterButton filters
        ]

viewFilterAndArgs : Int -> FilterAndArgs -> Element Msg
viewFilterAndArgs n filterAndArgs =
    row [ Border.color grey
        , Border.width 1
        ]
        [ text filterAndArgs.filter.desc
        , row [] <| Dict.values <| Dict.map (viewArg n) filterAndArgs.args
        , Input.button []
            { onPress = Just (RemoveFilter n)
            , label = text "x"
            }
        ]

viewArg : Int -> ArgKey -> ArgValue -> Element Msg
viewArg n name arg =
    Input.slider
        [ Element.height (Element.px 30)
        , Element.behindContent <|
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color grey
                , Border.rounded 2
                ]
                Element.none
        ]
        { min = toFloat arg.min
        , max = toFloat arg.max
        , value = toFloat arg.value
        , onChange = round >> (ArgSliderChanged n name)
        , label = Input.labelAbove [] (text name)
        , step = Just 1
        , thumb = Input.defaultThumb
        }

viewAddFilterButton : Filter -> Element Msg
viewAddFilterButton filter =
    let
        filterWithArgs =
            { filter = filter
            , args = filter.defaults }
    in
        row [ Border.color grey
            , Border.width 1
            ]
            [ text filter.desc
            , Input.button
                [ Background.color blue
                , Element.focused [ Background.color lightBlue ]
                ]
                { onPress = Just (AddFilter filterWithArgs)
                , label = text "+"
                }
            ]

port toElm : (D.Value -> msg) -> Sub msg
port toJs : D.Value -> Cmd msg
port base64ToJs : String -> Cmd msg
