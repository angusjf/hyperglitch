port module Main exposing (..)

import Json.Encode as E
import Browser
import Html exposing (Html, div, text, button, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)
import Json.Decode as D
import List
import Maybe
import Debug
import Glitches exposing (Image, Pixel, asdfPixelSort, brightness)
import List.Split

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
                newImage = asdfPixelSort (\p -> brightness p > 0.5) model.image
            in
                (model, toJs (encodeImage newImage))

subscriptions : Model -> Sub Msg
subscriptions model = toElm NewImage

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendToJS ] [ text "glitch!" ] ]

port toElm : (E.Value -> msg) -> Sub msg
port toJs : E.Value -> Cmd msg

{- ENCODE / DECODE -}

imageDecoder : D.Decoder Image
imageDecoder =
    D.map3 makeImage
        (D.at ["width"] D.int)
        (D.at ["height"] D.int)
        (D.at ["data"] (D.list D.int))

makeImage : Int -> Int -> List Int -> Image
makeImage width height data =
    { width  = width
    , height = height
    , data   = List.map groupList <| List.Split.chunksOfLeft (width * 4) data }

groupList : List Int -> List Pixel
groupList list = case list of
    r::g::b::a::rest -> {red = r, green = g, blue = b, alpha = a} :: groupList rest
    _ -> []

{-
pixelDecoder : D.Decoder Pixel
pixelDecoder =
    D.index 0 D.int
        |> D.andThen (\r -> D.index 1 D.int
        |> D.andThen (\g -> D.index 2 D.int
        |> D.andThen (\b -> D.index 3 D.int
        |> D.andThen (\a -> D.succeed {red = r, green = g, blue = b, alpha = a}))))
        -}

encodeImage : Image -> E.Value
encodeImage image = E.object
    [ ("width", E.int image.width)
    , ("height", E.int image.height)
    , ("data", encodeRow image.data) ]

encodeRow : List (List Pixel) -> E.Value
encodeRow data = E.list E.int <| List.concat <| List.map flattenRow data

flattenRow : List Pixel -> List Int
flattenRow pixels = case pixels of
    p::ps -> p.red::p.green::p.blue::p.alpha::flattenRow ps
    _     -> []

jsonToImage : E.Value -> Image
jsonToImage json = case D.decodeValue imageDecoder json of
    (Ok image)  -> image
    (Err error) -> Debug.todo <| D.errorToString error
