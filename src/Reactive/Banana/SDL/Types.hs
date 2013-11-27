-- | Types used for events
module Reactive.Banana.SDL.Types ( EventSource, SDLEventSource (..), WrappedEvent
                                 , TickEvent ) where

import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import Data.Word
import Reactive.Banana.Frameworks (AddHandler)

-- | Generic Event Source
type EventSource a = (AddHandler a, a -> IO ())
-- | an event containing a list of SDL event
type WrappedEvent t = R.Event t [SDL.Event]
-- | SDL Tick event
type TickEvent t = R.Event t Word32
-- | SDL Event Source
data SDLEventSource = SDLEventSource { getSDLEvent :: EventSource [SDL.Event]
                                     , getTickEvent :: EventSource Word32 }