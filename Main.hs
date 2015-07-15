{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Th

main = do
  -- main
  -- print [dump|1]
  print $(dump [|(a,False, True || False)|])
  -- runQ ([|(a,False)|]) >>= print
  where
    -- qexp = [|(a,False)|]
    a = True
