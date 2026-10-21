{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Common (write)
import Rapids
import Rapids.SVG

aframe = foldMap (pad 1) [svg| W 1,0 h 10 [ W 1 v 5 ] h 2  |]

aframeRef = scale 10 (-1) 1 unitCube + translate ex 9 (scale 2 5 1 unitCube) + translate 10 0 0 unitCylinder + translate 10 4 0 (scale 2 1 1 unitCube)  

$(write [| [$red aframe, translate ez 2 ($green aframeRef), translate ez 4 (aframeRef - aframe)] |] [| [aframe - aframeRef, aframeRef - aframe] |])
