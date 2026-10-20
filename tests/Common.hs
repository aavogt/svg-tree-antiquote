{-# LANGUAGE TemplateHaskell #-}

module Common (write) where

import Language.Haskell.TH (Dec, ExpQ, Loc (..), Q, location, litE, stringL)
import Rapids (volume, writeSTEPColor)

stepName :: ExpQ
stepName = do
  Loc {loc_filename} <- location
  let sourceName = reverse (takeWhile (/= '/') (reverse loc_filename))
  [| "step/" ++ takeWhile (/= '.') $(litE (stringL sourceName)) ++ ".step" |]

write :: ExpQ -> ExpQ -> Q [Dec]
write solid differences = [d|
  main = do
    writeSTEPColor $stepName $solid
    print (map volume $differences)
  |]
