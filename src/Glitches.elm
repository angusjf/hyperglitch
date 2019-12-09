module Glitches exposing (Image, Pixel, asdfPixelSort, brightness)

import Debug
import List
import List.Extra
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

{- PART 1 -}

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

asdfPixelSort : (Pixel -> Bool) -> Image -> Image
asdfPixelSort break img =
    let
        rowFns = List.repeat img.height (asdfRowSort break)
    in
        applyRowFnsToImage rowFns img

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
