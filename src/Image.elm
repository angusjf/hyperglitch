module Image exposing (Image, Pixel, imageToJson, jsonToImage, map, add)

import Json.Encode as E
import Json.Decode as D
import List.Split
import Debug
import List
import Maybe

type alias Image =
    { width  : Int
    , height : Int
    , data   : List (List Pixel) }

type alias Pixel =
    { red   : Int
    , green : Int
    , blue  : Int
    , alpha : Int }

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

imageToJson : Image -> E.Value
imageToJson image = E.object
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

map : (Pixel -> Pixel) -> Image -> Image
map fn img = { img | data = List.map (List.map fn) img.data }

map2 : (Pixel -> Pixel -> Pixel) -> Image -> Image -> Image
map2 fn img1 img2 =
    let
        go f list1 list2 =
            case (list1, list2) of
                (x::xs, y::ys) -> List.map2 f x y :: go f xs ys
                (_, _) -> []
    in
        { img1 | data = go fn img1.data img2.data }

add : Image -> Image -> Image
add image1 image2 = map2 addPixel image1 image2

addPixel : Pixel -> Pixel -> Pixel
addPixel p1 p2 =
    { red = p1.red + p2.red
    , green = p1.green + p2.green
    , blue = p1.blue + p2.blue
    , alpha = p1.alpha + p2.alpha
    }
