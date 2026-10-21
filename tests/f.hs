{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Common (write)
import Rapids
import Rapids.SVG

f = foldMap (pad 1) [svg| W 0.1 v 1 { W 0.5 h 1 } v 1 h 1 |]
fRef = foldMap (pad 1) [svg| W 0.1 v 1 v 1 h 1 |]
  + foldMap (pad 1) [svg| W 0.5 M 0,1 h 1 |]

$(write [| [$red fRef, translate ez 2 ($green f)] |] [| [f - fRef, fRef - f] |])
