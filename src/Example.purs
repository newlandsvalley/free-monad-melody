module Example (example) where

import Prelude (Unit, bind, ($))
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty (fromList)
import Data.List (List(..), (:))
import Data.Midi.Instrument (InstrumentName(..))
import Language (Melody, Channel, FontDef(..), ChordDef(..), NoteDef(..), note, rest, chord, soundFonts)

-- an example of the DSL supported by our language

achord :: Channel -> ChordDef
achord channel =
  unsafePartial go where
    go :: Partial => ChordDef
    go =
      case ( fromList $
        NoteDef { channel : channel
        , pitch : 57
        , duration : 0.8
        , gain : 1.0
        } :
        NoteDef { channel : channel
        , pitch : 60
        , duration : 0.8
        , gain : 1.0
        } :
        NoteDef { channel : channel
        , pitch : 64
        , duration : 0.8
        , gain : 1.0
        } :
        Nil
      ) of
        Just l -> ChordDef l

fonts :: List InstrumentName -> FontDef
fonts fs =
  unsafePartial go where
    go :: Partial => FontDef
    go =
      case ( fromList fs ) of
        Just f -> FontDef f

example :: Melody Unit
example = do
  _ <- soundFonts $ fonts ( TangoAccordion : AcousticGrandPiano : Nil )
  _ <- chord $ achord 0
  _ <- note $ NoteDef { channel : 0
       , pitch : 64
       , duration : 0.8
       , gain : 1.0
       }
  _ <- rest 0.1
  _ <- note $ NoteDef { channel : 0
       , pitch : 66
       , duration : 0.4
       , gain : 1.0
       }
  _ <- rest 0.1
  _ <- note $ NoteDef { channel : 0
       , pitch : 64
       , duration : 0.4
       , gain : 1.0
       }
  _ <- rest 0.1
  _ <- note $ NoteDef { channel : 0
       , pitch : 62
       , duration : 0.4
       , gain : 1.0
       }
  chord $ achord 1
