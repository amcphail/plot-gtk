-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Plot
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.Plot (
                            ) where

-----------------------------------------------------------------------------

import Graphics.UI.Gtk

import Graphics.Rendering.Plot.Figure

import Graphics.Rendering.Plot.Render

-----------------------------------------------------------------------------

plotNew :: Frame -> Figure () -> IO Plot
plotNew frame figure = do
   canvas <- drawingAreaNew
   containerAdd frame canvas
   widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

   -- unclear here
   {-
     newAttr (\this -> getFigureData this->uniqueID) 
             (\this fig -> setFigureData this->uniqueID fig)
   -}

   on canvas exposeEvent $ tryEvent $ do s <- liftIO $ widgetGetSize canvas
                                         drw <- liftIO $ widgetGetDrawWindow canvas
                                         liftIO $ renderWithDrawable drw (render figure s)

-----------------------------------------------------------------------------

