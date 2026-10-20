{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Common (write)
import Rapids
import Rapids.SVG

aframe = foldMap (pad 1) [svg| W 1,0 h 10 [ w 2 v 5 ] l 5,5 5,-5 w 2 [ v -5 ] h 10 |]

aframeRef = foldMap (pad 1) [svg| W 1,0 h 10 W 2 v 5 W 1,0 l 5,5 5,-5 W 2 v -5 W 1,0 h 10 |]

$(write [| $red aframe + translate ez 2 ($green aframeRef) |] [| [aframe - aframeRef, aframeRef - aframe] |])
