module Language where

import Prelude (class Functor, class Show, Unit, show, unit, (<>), ($))
import Control.Monad
import Control.Monad.Free (Free, liftF)
import Data.List.Types (NonEmptyList)
import Data.Foldable (intercalate)
import Data.Midi.Instrument (InstrumentName, gleitzmanName)

type Channel = Int
type Pitch = Int
type Duration = Number
type Gain = Number

newtype FontDef = FontDef (NonEmptyList InstrumentName)

instance showFontDef :: Show FontDef where
  show (FontDef fd) =
    intercalate " , " $ map gleitzmanName fd

newtype NoteDef = NoteDef
    { channel :: Channel
    , pitch:: Pitch
    , duration :: Duration
    , gain :: Gain
    }

shownd :: NoteDef -> String
shownd (NoteDef nd) =
  "channel: " <> (show nd.channel) <> ", " <>
  "pitch: " <> (show nd.pitch) <> ", " <>
  "duration: " <> (show nd.duration) <> ", " <>
  "gain: " <> (show nd.gain)

instance showNoteDef :: Show NoteDef where
  show nd = shownd nd

newtype ChordDef = ChordDef (NonEmptyList NoteDef)

instance showChordDef :: Show ChordDef where
  show (ChordDef cd) =
    "[ " <>
    (intercalate " ; " $ map shownd cd) <>
    " ]"

data MelodyF a =
    SoundFonts FontDef a
  | Note NoteDef a
  | Rest Duration a
  | Chord ChordDef a

derive instance functorMelodyF :: Functor MelodyF

-- | the free monad
type Melody = Free MelodyF

-- | smart constructors

soundFonts :: FontDef -> Melody Number
soundFonts (FontDef f) = liftF (SoundFonts (FontDef f) 0.0)

rest :: Duration -> Melody Unit
rest d = liftF (Rest d unit)

note :: NoteDef -> Melody Unit
note (NoteDef n) = liftF (Note (NoteDef n) unit)

chord :: ChordDef -> Melody Unit
chord c = liftF (Chord c unit)
