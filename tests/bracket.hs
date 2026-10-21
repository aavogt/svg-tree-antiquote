{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Common
import Rapids
import Rapids.SVG

a = foldMap (pad 1) [svg| W 1,0 M 31/2,0 H 6.5 v 3 { H 0 } v 1  |]
b = foldMap (pad 1) [svg| W 1,0 M 31/2,0 H 6.5 v 3 { W 1,0 H 0 } v 1  |]

write [| [$red a, translate ez 2 b] |]
  [| let solids = [$red a, translate ez 2 b]
         differences = [a - b, b - a]
     in if all ((== Nothing) . checkSolid) solids
             && all ((== 0) . volume) differences
          then differences
          else error "bracket solids must be valid and equivalent"
   |]
