{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Common (write
#ifdef DEBUG_OFFSET
  , svgDebug
#endif
  ) where

import Language.Haskell.TH (Dec, ExpQ, Loc (..), Q, location, litE, stringL)
import Rapids (volume, writeSTEPColor)
#ifdef DEBUG_OFFSET
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import SvgTreeAntiquote (parsePathCommandsSupplement)
import SvgTreeOffset (pathOutline)
import Waterfall.SVG (convertPathCommands)
#endif
#ifdef DEBUG_OFFSET
svgDebug :: QuasiQuoter
svgDebug =
  QuasiQuoter
    { quoteExp = \str ->
        [| either (error . show) id $ convertPathCommands $ pathOutline (0.1 :: Double) $(parsePathCommandsSupplement str) |]
    }
#endif

stepName :: ExpQ
stepName = do
  Loc {loc_filename} <- location
  let sourceName = reverse (takeWhile (/= '/') (reverse loc_filename))
  [| "step/" ++ takeWhile (/= '.') $(litE (stringL sourceName)) ++ ".step" |]

write :: ExpQ -> ExpQ -> Q [Dec]
write solid differences = [d|
  main = do
    writeSTEPColor $stepName (unions $solid)
    print (map checkSolid $solid)
    print (map volume $differences)
  |]


