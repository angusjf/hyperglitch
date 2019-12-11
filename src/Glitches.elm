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

filters : List Filter
filters =
    [ Filter "asdf pixel sort" (asdfPixelSort (\p -> brightness p > 127))
    , Filter "row shift" shift
    , Filter "colors" colors
    , Filter "bleed" bleed
    ]

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
        (randomDegrees, s1) = Random.step (Random.list 999 (Random.int -360 360)) s
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
        (rands2d, s1) = Random.step (Random.list 99 (Random.list 99 (Random.int 5 15))) s
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

colors : (Image, Random.Seed) -> (Image, Random.Seed)
colors (img, s) =
    let
        (randomColors, s1) = Random.step (Random.list 999 (Color.gen)) s
        (draggedRowFns, s2) = randomRepeatElems (Random.int 1 10) s1 rowFns
        rowFns = List.repeat 100 identity --List.map (List.map (Color.keep)) randomColors
    in
        (applyRowFnsToImage rowFns img, s2)
