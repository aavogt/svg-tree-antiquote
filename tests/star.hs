{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Common (write)
import Rapids
import Rapids.SVG

star = foldMap (pad 1) [svg| W 0.1 { l 2,0  } { l 0,2 } { l -2,0 } { l -2,0 } |]

starRef = rotated ez pi ez (pi/2) $ foldMap (pad 1) [svg| W 0.1 l 2,0 |]

$(write
  [| [ $red $ star, translate ez 1 $ $green starRef] |]
  [| [star - starRef, starRef - star] |])
