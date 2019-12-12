module Glitches exposing (Filter, filters)

import Debug
import List
import List.Extra
import Maybe
import Image exposing (Image, Pixel)
import Random
import Color

type alias Glitch = ((Image, Random.Seed) -> (Image, Random.Seed))

type alias Filter =
    { desc   : String
    , glitch : Glitch
    }

filters : List (String, List Filter)
filters =
    let
        horizontals =
            [ Filter "asdf pixel sort" (asdfPixelSort (\p -> brightness p > 127))
            , Filter "row shift" shift
            , Filter "colors" colors
            , Filter "bleed" bleed
            ]
        verticals = List.map (\f -> {f | desc = "v" ++ f.desc, glitch = verticalize f.glitch}) horizontals
        rgbs = List.map (\f -> {f | desc = "rgb" ++ f.desc, glitch = rgbize f.glitch}) horizontals
    in
        [ ("horizontal functions", horizontals)
        , ("vertical functions", verticals)
        , ("rgb functions", rgbs) ]

{- PART 1 -}

rgb : Image -> (Image, Image, Image)
rgb img =
    let
        red   = Image.map (Color.keep Color.Red)   img
        green = Image.map (Color.keep Color.Green) img
        blue  = Image.map (Color.keep Color.Blue)  img
    in
        (red, green, blue)

applyRgb : (Glitch, Glitch, Glitch) -> Glitch
applyRgb (f1, f2, f3) =
    \(img, s) ->
        let
            (red, green, blue) = rgb img
            (r, s1) = f1 (red, s)
            (g, s2) = f2 (green, s1)
            (b, s3) = f3 (blue, s2)
        in
            (Image.add (Image.add r g) b, s3)

map : (Image -> Image) -> Glitch -> Glitch
map fn glitch = \(img, s) -> (fn img, s)

rgbize : Glitch -> Glitch
rgbize glitch = applyRgb (glitch, glitch, glitch)

add : Glitch -> Glitch -> Glitch
add g1 g2 =
    \(img, seed) ->
        let
            (out1, s1) = g1 (img, seed)
            (out2, s2) = g2 (out1, s1)
            out = Image.add out1 out2
        in
            (out, s2)

brightness : Pixel -> Float
brightness pixel =
    let
        (r, g, b) = ( toFloat pixel.red
                    , toFloat pixel.green
                    , toFloat pixel.blue )
    in
        (r + g + b) / 3

luminosity : Pixel -> Float
luminosity pixel =
    let
        (r, g, b) = ( toFloat pixel.red
                    , toFloat pixel.green
                    , toFloat pixel.blue )
        max = Maybe.withDefault 255 (List.maximum [r, g, b])
        min = Maybe.withDefault 0 (List.minimum [r, g, b])
    in
        (max + min) / 2

saturation : Pixel -> Float
saturation pixel =
    let
        (r, g, b) = ( toFloat pixel.red
                    , toFloat pixel.green
                    , toFloat pixel.blue )
        l = luminosity pixel
        max = Maybe.withDefault 1 (List.maximum [r, g, b])
        min = Maybe.withDefault 0 (List.minimum [r, g, b])
    in
        if l < 1
            then (max - min) / (1 - abs (2 * l - 1))
            else 0

{- PART 2 -}

applyRowFnsToImage : List (List Pixel -> List Pixel) -> Image -> Image
--applyRowFnsToImage fns img = {img | data = List.map2 (<|) fns img.data }
applyRowFnsToImage fns img = {img | data = applyRowFnsToRows fns img.data }

applyRowFnsToRows fs rs = case rs of
    row::rows -> case fs of
                    fn::fns -> (fn row) :: applyRowFnsToRows fns rows
                    []      -> []
    []        -> []

breakUp : (a -> Bool) -> List a -> List (List a)
breakUp fn = swapApply (List.Extra.span fn, List.Extra.break fn)

swapApply (f, g) xs = case f xs of
    ([], end)    -> swapApply (g, f) end
    (start, [])  -> start :: []
    (start, end) -> start :: swapApply (g, f) end

{- PART 3 -}

asdfPixelSort : (Pixel -> Bool) -> (Image, Random.Seed) -> (Image, Random.Seed)
asdfPixelSort break (img, seed) =
    let
        rowFns = List.repeat img.height (asdfRowSort break)
    in
        (applyRowFnsToImage rowFns img, seed)

asdfRowSort : (Pixel -> Bool) -> List Pixel -> List Pixel
asdfRowSort break = let sortFn a b = compare (brightness a) (brightness b) in
    List.concatMap (List.sortWith sortFn) << breakUp break

toBlack : Image -> Image
toBlack img =
    let
        black : Pixel
        black = { red = 255, green = 0, blue = 0, alpha = 255 }
    in
        {img | data = List.map (List.map (always black)) img.data }

rowToBlack : List Pixel -> List Pixel
rowToBlack =
    let
        black : Pixel
        black = { red = 255, green = 0, blue = 0, alpha = 255 }
    in
        List.map (always black)

toTestImage : Image -> Image
toTestImage img =
    let
        black : Pixel
        black = { red = 0, green = 0, blue = 0, alpha = 255 }
        red : Pixel
        red = { red = 255, green = 0, blue = 0, alpha = 255 }
        myFn : List Pixel -> List Pixel
        myFn xs = List.Extra.cycle (List.length xs) [red, black]
    in
        {img | data = List.map myFn img.data }

shift : (Image, Random.Seed) -> (Image, Random.Seed)
shift (img, s) =
    let
        (randomDegrees, s1) = Random.step (Random.list img.height (Random.int -360 360)) s
        (draggedRowFns, s2) = randomRepeatElems (Random.int 1 10) s1 rowFns
        rowFns = List.map rotate randomDegrees
    in
        (applyRowFnsToImage draggedRowFns img, s2)

randomRepeatElems : (Random.Generator Int) -> Random.Seed
    -> List a -> (List a, Random.Seed)
randomRepeatElems gen s xs =
    case xs of
        []    -> ([], s)
        x::rest ->
            let
                (rand, s1) = Random.step gen s
                (repeatedRest, s2) = randomRepeatElems gen s1 rest
            in
                (List.repeat rand x ++ repeatedRest, s2)

rotate : Int -> List a -> List a
rotate n xs = (List.drop n xs) ++ (List.reverse (List.take n xs))

bleed : (Image, Random.Seed) -> (Image, Random.Seed)
bleed (img, s) =
    let
        (rands2d, s1) = Random.step (Random.list img.height (Random.list img.width (Random.int 5 15))) s
    in
        (applyRowFnsToImage (List.map repeatElemsSafe rands2d) img, s1)

repeatElemsSafe : List Int -> List a -> List a
repeatElemsSafe numbers elems =
    case (numbers, elems) of
        (n::ns, x::_)  ->
            let
                repeats = List.take (List.length elems) <| List.repeat n x
                rest    = List.drop n elems
            in
                repeats ++ repeatElemsSafe ns rest
        _ -> elems

transpose : Image -> Image
transpose img =
    { img | width = img.height
    , height = img.width
    , data = List.Extra.transpose img.data }

colors : Glitch
colors (img, s) =
    let
        (randomColors, s1) = Random.step (Random.list 999 (Color.gen)) s
        rowFns = List.repeat 999 (\row -> List.map2 Color.keep randomColors row)
    in
        (applyRowFnsToImage rowFns img, s1)

verticalize : ((Image, Random.Seed) -> (Image, Random.Seed)) -> ((Image, Random.Seed) -> (Image, Random.Seed))
verticalize glitch =
    \(img, seed) ->
        let
            (output, seed1) = glitch (transpose img, seed)
        in
            (transpose output, seed1)
