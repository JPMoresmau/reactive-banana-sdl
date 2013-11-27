{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances
    , TemplateHaskell, TypeOperators #-}
-- | Types for graphics handling
module Reactive.Banana.SDL.Graphics.Types where

import Reactive.Banana as R
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF
import Data.Lens.Common
import Data.Lens.Template

-- | alias for surface
type Screen = SDL.Surface

-- | alias for an operation to draw on a surface, returning the drawn rectangle area if relevant
newtype Graphic = Graphic { paintGraphic :: Screen -> IO (Maybe Rect) }
-- | graphic operation on a rectangle
type GraphicOpt = Rect -> Graphic
-- | Graphic update
type GraphicUpdate = (GraphicOpt, Rect)
-- | Alignment 
data Alignment=Start | Middle | End
-- | Mask coordinates and optional clipping rectangle
data Mask = Mask { _maskClip :: Maybe Rect, _maskX :: Int, _maskY :: Int }
-- | color fill
data Fill = Fill { _fillClip :: Maybe Rect, _fillColor :: Color }
-- | Standard Text
data Text = Text { _textMsg :: String, _textFont :: Font, _textColor :: Color }
-- | Aligned Text
data AlignedText = AlignedText {_atextText :: Text, _atextHAlign :: Alignment, _atextVAlign :: Alignment}
-- | Image
data Image = Image { _imagePath :: String}

$(makeLenses [''Mask, ''Fill, ''Text, ''AlignedText, ''Image])

instance Eq Color where
    (Color r1 g1 b1) == (Color r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2

instance Show Color where
    show (Color r g b) = "Color { " ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ " }"

-- | the draw class for involved graphics
class Draw canvas mask where
    draw :: canvas -> mask -> Graphic