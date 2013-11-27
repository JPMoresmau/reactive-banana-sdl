-- | Functions on events
module Reactive.Banana.SDL.Util ( addHandler, fire, sdlEvent, tickEvent, tickDiffEvent
                                , keyEvent, keyDownEvent, keyUpEvent, mouseEvent, mouseButtonEvent
                                , filterEq, keyFilter, keyUpFilter
                                , mouseEventWithin, keyPressed, buttonClick
                                , whileM, successive ) where

import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import Reactive.Banana.SDL.Types
import Control.Monad (when,liftM)
import Reactive.Banana.Frameworks (AddHandler, Frameworks, fromAddHandler)

-- | run while the given computation returns True
whileM :: IO Bool -> IO ()
whileM f = f >>= (\x -> when x $ whileM f )

-- | get the AddHandler from a EventSource
addHandler :: EventSource a -> AddHandler a
addHandler = fst

-- | fire the event from an Event Source
fire :: EventSource a -> a -> IO ()
fire = snd

-- | SDL event
sdlEvent :: Frameworks t => SDLEventSource -> Moment t (WrappedEvent t)
sdlEvent = fromAddHandler . addHandler . getSDLEvent

-- | SDL tick
tickEvent :: Frameworks t => SDLEventSource -> Moment t (TickEvent t)
tickEvent = fromAddHandler . addHandler . getTickEvent

-- | event carrying the difference between the last two SDL ticks
tickDiffEvent :: Frameworks t =>SDLEventSource -> Moment t (TickEvent t)
tickDiffEvent =liftM (successive (\a b->if b>a then Just (b-a) else Nothing)) . tickEvent
 
-- | filter any key events
keyEvent :: WrappedEvent t -> WrappedEvent t
keyEvent = collect . filterE isKey . spill
    where
        isKey e = case e of
            KeyUp _ -> True
            KeyDown _ -> True
            otherwise -> False

-- | event carrying the key pressed down 
keyDownEvent :: WrappedEvent t -> R.Event t SDL.Keysym
keyDownEvent= filterJust . (isDown <$>) . spill . keyEvent 
        where isDown (SDL.KeyDown k)=Just k
              isDown _ = Nothing

-- | event carrying the key pressed up 
keyUpEvent :: WrappedEvent t -> R.Event t SDL.Keysym
keyUpEvent= filterJust . (isDown <$>) . spill . keyEvent 
        where isDown (SDL.KeyUp k)=Just k
              isDown _ = Nothing

-- | filter any mouse event (button or move)
mouseEvent :: WrappedEvent t -> WrappedEvent t
mouseEvent esdl = mouseMotion `union` mouseButtonEvent esdl
    where
        mouseMotion = collect . filterE isMotion $ spill esdl
        isMotion e = case e of
            MouseMotion {} -> True
            otherwise -> False

-- | mouse button event
mouseButtonEvent :: WrappedEvent t -> WrappedEvent t
mouseButtonEvent = collect . filterE isButton . spill
    where
        isButton e = case e of
            MouseButtonDown {} -> True
            MouseButtonUp {} -> True
            otherwise -> False

-- | mouse event occuring inside a given area
mouseEventWithin :: Rect -> WrappedEvent t -> WrappedEvent t
mouseEventWithin ~(Rect x y w h) = collect . filterE isWithin . spill
    where
        within mx' my' = let (mx, my) = (fromIntegral mx', fromIntegral my') in (mx >= x && mx <= x + w) && (my >= y && my <= y + h)
        isWithin e = case e of
            MouseMotion mx my _ _ -> within mx my
            MouseButtonDown mx my _ -> within mx my
            MouseButtonUp mx my _ -> within mx my
            otherwise -> False

filterEq :: Eq a => R.Event t a -> R.Event t a
filterEq = filterJust . fst . mapAccum Nothing . fmap f
    where
        f y (Just x) = if x == y then (Nothing, Just x) else (Just y, Just y)
        f y Nothing  = (Just y, Just y)

-- | filter an event on a particular key being held down
keyFilter :: SDL.SDLKey -> SDL.Event -> Bool
keyFilter k (KeyDown (Keysym k' _ _)) | k == k' = True
keyFilter _ _ = False

-- | filter an event on a particular key being released
keyUpFilter :: SDL.SDLKey -> SDL.Event -> Bool
keyUpFilter k (KeyUp (Keysym k' _ _)) | k == k' = True
keyUpFilter _ _ = False

-- | filter if the function on two successive 'a's return a Just value
successive :: (a -> a -> Maybe b) -> R.Event t a -> R.Event t b
successive f e = filterJust (b <@> e)
    where b = stepper (const Nothing) (f <$> e)

-- | fires when the given key is pressed (down + up)
keyPressed :: SDL.SDLKey -> WrappedEvent t -> WrappedEvent t
keyPressed k = collect . successive (\p c -> if keyFilter k p && keyUpFilter k c then Just c else Nothing) . spill . keyEvent

-- | fires when the specific button if clicked (down and up)
buttonClick :: MouseButton -> WrappedEvent t -> WrappedEvent t
buttonClick b = collect . successive sameButton . spill . mouseButtonEvent
    where sameButton (MouseButtonDown _ _ b1) e@(MouseButtonUp _ _ b2) | b1 == b && b2 == b = Just e
          sameButton _ _ = Nothing
