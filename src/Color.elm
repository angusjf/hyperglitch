module Color exposing (..)

import Random
import Image

type Color  =  Red | Green | Blue

gen : Random.Generator Color
gen  =  Random.uniform Red [Green, Blue]

keep : Color -> Image.Pixel -> Image.Pixel
keep color pixel =
    case color of
        Red   -> { red = pixel.red
                 , green = 0
                 , blue = 0
                 , alpha = pixel.alpha }
        Green -> { red = 0
                 , green = pixel.green
                 , blue = 0
                 , alpha = pixel.alpha }
        Blue  -> { red = 0
                 , green = 0
                 , blue = pixel.blue
                 , alpha = pixel.alpha }

lose : Color -> Image.Pixel -> Image.Pixel
lose color pixel =
    case color of
        Red   -> { red = 0
                 , green = pixel.green
                 , blue = pixel.blue
                 , alpha = pixel.alpha }
        Green -> { red = pixel.red
                 , green = 0
                 , blue = pixel.blue
                 , alpha = pixel.alpha }
        Blue  -> { red = pixel.red
                 , green = pixel.green
                 , blue = 0
                 , alpha = pixel.alpha }
