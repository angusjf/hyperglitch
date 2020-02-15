module Filters exposing (Filter, filters, ArgKey, ArgValue, ArgDict)

import Glitches
import Set
import Dict
import Image exposing (..)

type alias ArgKey = String
type alias ArgValue =
    { min : Int
    , max : Int
    , value : Int
    }

type alias ArgDict = Dict.Dict ArgKey ArgValue

type alias Filter =
    { desc     : String
    , defaults : ArgDict
    , glitch   : ArgDict -> Maybe Glitches.Glitch
    }

shiftFilter : Filter
shiftFilter =
    { desc = "Shift"
    , defaults = Dict.fromList [ ("amount", { min = 0, max = 36, value = 64 }) ]
    , glitch =
        \args ->
            case Dict.get "amount" args of
                Just amount -> Just (Glitches.shift amount.value)
                Nothing     -> Nothing
    }

asdfFilter : Filter
asdfFilter =
    { desc = "ASDF Pixel Sort"
    , defaults = Dict.fromList [ ("luminosity", { min = 0, max = 128, value = 255 }) ]
    , glitch =
        \args ->
            case Dict.get "luminosity" args of
                Just a  -> Just (Glitches.asdfPixelSort (\p -> luminosity p > (toFloat a.value)))
                Nothing -> Nothing
    }

filters : List (String, List Filter)
filters =
    let
        horizontals =
            [ shiftFilter
            , asdfFilter
            ]
            -- [ Filter "asdf pixel sort" (asdfPixelSort (\p -> luminosity p > 127))
            -- , Filter "shift" shift
            -- , Filter "colors" colors
            -- , Filter "bleed" bleed
            -- ]
        --verticals = List.map (\f -> {f | desc = "v" ++ f.desc, glitch = Glitches.verticalize f.glitch}) horizontals
        --rgbs = List.map (\f -> {f | desc = "rgb" ++ f.desc, glitch = Glitches.rgbize f.glitch}) horizontals
    in
        [ ("horizontal functions", horizontals) ]
        --, ("vertical functions", verticals)
        --, ("rgb functions", rgbs) ]
