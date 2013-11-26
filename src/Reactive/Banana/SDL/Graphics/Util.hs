{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module Reactive.Banana.SDL.Graphics.Util where

import Reactive.Banana.SDL.Graphics.Types

import Reactive.Banana as R
import Graphics.UI.SDL as SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL (flip)
import Graphics.UI.SDL.TTF
import Data.Lens.Common
import Graphics.UI.SDL.Image (load)
import Reactive.Banana.Frameworks (Frameworks, changes, reactimate)
import Control.Monad (void)
--import Debug.Trace

over :: Graphic -> Graphic -> Graphic
(Graphic x) `over` (Graphic y) = Graphic $ \surface -> y surface >> x surface

under :: Graphic -> Graphic -> Graphic
under = flip over

emptyG :: Graphic
emptyG = Graphic $ \_ -> return ()

render :: Graphic -> Graphic
render (Graphic x) = Graphic $ \surface -> x surface >> SDL.flip surface

withinBox :: Rect -> Graphic -> GraphicOpt
withinBox r g r'=
    if r `intersect` r' then emptyG else g

overOpt :: GraphicOpt -> GraphicOpt -> GraphicOpt
overOpt g1 g2 r =  g1 r `over` g2 r

overUpdate :: GraphicOpt -> GraphicUpdate -> GraphicUpdate
overUpdate g1 (g2,r) = (g1 `overOpt` g2, r)

intersect :: Rect -> Rect -> Bool
intersect r1 r2 = xintersect && yintersect
    where
        xintersect = x1 `between` (x2, w2) || w1 `between` (x2,w2)
        yintersect = y1 `between` (y2,h2) || h1 `between` (y2,h2)
        x1 = rectX r1
        x2 = rectX r2
        y1 = rectY r1
        y2 = rectY r2
        w1 = x1 + rectW r1
        w2 = x2 + rectW r2
        h1 = y1 + rectH r1
        h2 = y2 + rectH r2

between :: Int -> (Int,Int) -> Bool
between x (l,h) = x >= l && x <= h

instance Draw SDL.Surface Mask where
    draw src mask = Graphic $ \dst -> void $ blitSurface src clip dst offset
        where
            clip = maskClip ^$ mask
            offset = Just Rect { rectX = maskX ^$ mask, rectY = maskY ^$ mask, rectW = 0, rectH = 0 }

instance Draw Fill Mask where
    draw fill mask = Graphic $ \dst -> pixel dst >>= \c -> void $ fillRect dst clip c
        where
            pixel dst = (mapRGB . surfaceGetPixelFormat) dst (colorRed color) (colorGreen color) (colorBlue color)
            clip = fillClip ^$ fill
            color = fillColor ^$ fill

instance Draw Text Mask where
    draw text mask = Graphic $ \dst -> void $ blitText dst
        where
            blitText dst = do
                --sz <- textSize (textFont ^$ text) (textMsg ^$ text)
                txt <- renderTextBlended (textFont ^$ text) (textMsg ^$ text) (textColor ^$ text)
                blitSurface txt clip dst offset
                freeSurface txt
            clip = maskClip ^$ mask
            offset = Just Rect { rectX = maskX ^$ mask, rectY = maskY ^$ mask, rectW = 0, rectH = 0 }

instance Draw Image Mask where
    draw img mask = Graphic $ \dst -> void $ blitText dst
        where
            blitText dst = do
                --sz <- textSize (textFont ^$ text) (textMsg ^$ text)
                is<-load $ imagePath ^$ img
                blitSurface is clip dst offset
                freeSurface is
            clip = maskClip ^$ mask
            offset = Just Rect { rectX = maskX ^$ mask, rectY = maskY ^$ mask, rectW = 0, rectH = 0 }


instance Draw AlignedText Rect where
    draw text rect = Graphic $ \dst -> void $ blitText dst
        where
            blitText dst = do
                tr<-getTextRect text rect
                let offset = Just Rect { rectX = rectX tr, rectY = rectY tr, rectW = 0, rectH = 0 }
                txt <- renderTextBlended (textFont ^$  atextText ^$ text) (textMsg ^$  atextText ^$ text) (textColor ^$ atextText ^$ text)
                blitSurface txt Nothing dst offset
                freeSurface txt
            
getTextRect :: AlignedText -> Rect -> IO Rect
getTextRect text rect=do
  (w,h) <- textSize (textFont ^$ atextText ^$ text) (textMsg ^$ atextText ^$ text)
  let x=case atextHAlign ^$ text of
          Start->rectX rect
          Middle->rectX rect+((rectW rect - w) `div` 2)
          End->rectX rect + rectW rect-w
  let y=case atextVAlign ^$ text of
          Start->rectY rect
          Middle->rectY rect+((rectY rect - h) `div` 2)
          End->rectY rect + rectY rect-h     
  return  Rect { rectX = x, rectY = y, rectW = w, rectH = h }
   
renderGraph :: Frameworks t => Behavior t Graphic -> Behavior t Screen -> Moment t ()
renderGraph bgraph bscreen = do
    egraph <- changes $ render <$> bgraph
    reactimate $ flip paintGraphic <$> bscreen <@> egraph

renderGraphOnEvent :: Frameworks t => Behavior t Graphic -> Behavior t Screen -> R.Event t a -> Moment t ()
renderGraphOnEvent bgraph bscreen event =
    reactimate $ paintGraphic <$> render <$> bgraph <*> bscreen <@ event