{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Common (write)
import Rapids
import Rapids.SVG

ell = foldMap (pad 1) [svg| W 0.1,0 h 1 W 0,0.2 v 1 |]
ellRef = foldMap (pad 1) [svg| W 0.1,0 h 1 |]
  + foldMap (pad 1) [svg| W 0,0.2 M 1,0 v 1 |]
  - translate ex (1-0.1) (scale 0.1 1 (translate ey (-1) unitCube - unitCylinder))

$(write [| $red ellRef + translate ez 2 ($green ell) |]
  [| [ellRef - ell, ell - ellRef] |])
