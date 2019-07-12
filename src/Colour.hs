module Colour
( Colour(..)
, colour
, hadamard
, colourToRGB
) where

import Linear

type Colour = V3 Double

colour :: Double -> Double -> Double -> Colour
colour r g b = V3 r g b

hadamard :: Colour -> Colour -> Colour
hadamard (V3 r1 g1 b1) (V3 r2 g2 b2) = V3 (r1*r2) (g1*g2) (b1*b2)

colourToRGB :: Colour -> [String]
colourToRGB (V3 r g b) = [(rgb r), (rgb g), (rgb b)]
    where rgb = \d -> show . floor $ d * 255
