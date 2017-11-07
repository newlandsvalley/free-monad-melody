module Main where

import Prelude (Unit, bind, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Interpreter (runPlayMelody)
import Example (example)
import Audio.SoundFont (AUDIO)
import Control.Monad.Aff (launchAff, liftEff')
import Network.HTTP.Affjax (AJAX)

main :: forall e.
        Eff
          ( ajax :: AJAX
          , au :: AUDIO
          , console :: CONSOLE
          , exception :: EXCEPTION
          | e
          )
          Unit
main =
  do
    _ <- launchAff do
      _ <- runPlayMelody example
      liftEff' $ log "play finished"
    log "setup finished"
