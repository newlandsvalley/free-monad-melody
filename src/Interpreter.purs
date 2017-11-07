module Interpreter where

import Language

import Audio.SoundFont (AUDIO, Instrument, MidiNote, loadRemoteSoundFonts, playNote, playNotes)
import Data.Midi.Instrument (InstrumentName)
import Control.Monad.Aff (Aff, liftEff', delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Trans (StateT, evalStateT, put, get)
import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty (toUnfoldable)
import Data.NaturalTransformation (type (~>))
import Data.Time.Duration (Milliseconds(..))
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, const, discard, map, pure, show, ($), (<$>), (<>), (*))

-- | interpreters must be provided as natural transformations
-- | i.e. they can't alter the r

-- | this interpreter just logs the melody
logMelody :: forall eff. MelodyF ~> (Eff (console :: CONSOLE | eff))
logMelody (SoundFonts f r) =
  const r <$> log ("soundfonts: " <> (show f))
logMelody (Note n r) =
  const r <$> log ("note: " <> (show n))
logMelody (Rest d r) =
  const r <$> log ("rest: " <> (show d))
logMelody (Chord c r) =
  const r <$> log ("chord: " <> (show c))

runLogMelody :: forall eff. Melody ~> (Eff (console :: CONSOLE | eff))
runLogMelody = foldFree logMelody

-- | we will play the melody by interpreting in the Aff monad
-- | however, because we need to save the instruments to state after loading
-- | and before we can use them to play a note, we wrap Aff inside StateT
type AudioState eff r = StateT (Array Instrument) (Aff eff) r

-- | this interpreter plays the melody
-- | each MIDI note plays asynchronously within web-audio
-- | so we need to delay for the corrext duration after each note is played
playMelody :: forall eff. MelodyF ~> AudioState (ajax :: AJAX, au :: AUDIO | eff)
playMelody (SoundFonts fontdefs r) =
  do
    instruments <- lift $ loadRemoteSoundFonts (buildFontDefs fontdefs)
    put instruments
    pure r
playMelody (Note n r) =
  do
    instruments <- get
    dur <- lift $ liftEff' $ playNote instruments (buildNote n)
      -- _ <- runPlayMelody example
    _ <- lift $ delay (Milliseconds $ 1000.0 * dur)
    pure r
playMelody (Rest d r) =
  do
    _ <- lift $ delay (Milliseconds $ 1000.0 * d)
    pure r
playMelody (Chord c r) =
  do
    instruments <- get
    dur <- lift $ liftEff' $ playNotes instruments (buildChord c)
    _ <- lift $ delay (Milliseconds $ 1000.0 * dur)
    pure r

-- | run the playMelody transform, supplying, as initial state,
-- | an empty array of Instruments
runPlayMelody :: forall eff. Melody ~> Aff (ajax :: AJAX, au :: AUDIO | eff)
runPlayMelody melody =
  evalStateT (foldFree playMelody melody) []

-- | these just translate from the DSL Language to the SoundFont API
buildFontDefs :: FontDef -> Array InstrumentName
buildFontDefs (FontDef fd) =
  toUnfoldable fd

buildNote :: NoteDef -> MidiNote
buildNote (NoteDef nd) =
  { channel : nd.channel, id : nd.pitch, timeOffset : 0.0, duration : nd.duration, gain : nd.gain }

buildRest :: Duration -> MidiNote
buildRest duration =
  { channel : 0, id : 0, timeOffset : 0.0, duration : duration, gain : 0.0 }

buildChord :: ChordDef -> Array MidiNote
buildChord (ChordDef cd) =
  toUnfoldable $ map buildNote cd
