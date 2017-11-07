module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Interpreter (runLogMelody)
import Example (example)
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  runLogMelody example
