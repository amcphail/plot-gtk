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
-- A 'DrawingArea' widget that displays 'Figure's
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.Plot (
                             FigureHandle()
                             -- * Drawing Area
                             , plotNew
                             -- * Attributes
                             , figure
                            ) where

-----------------------------------------------------------------------------

import System.IO.Unsafe

import Control.Concurrent.MVar

import Control.Monad.Trans

import System.Glib.GObject

import Graphics.UI.Gtk

import Graphics.Rendering.Plot.Figure

import Graphics.Rendering.Plot.Render

-----------------------------------------------------------------------------

type FigureHandle = MVar FigureState

-----------------------------------------------------------------------------

-- | create a new 'Figure' plot
plotNew :: FigureHandle -> IO DrawingArea
plotNew f = do
   canvas <- drawingAreaNew
   
   set canvas [maybeFigure := (Just f)]

   _ <- on canvas exposeEvent $ tryEvent $ do s <- liftIO $ widgetGetSize canvas
                                              drw <- liftIO $ widgetGetDrawWindow canvas
                                              fig <- liftIO $ get canvas figure 
                                              liftIO $ renderWithDrawable drw (renderFigureState fig s)

   return canvas

-----------------------------------------------------------------------------

-- | the figure attribute
figure :: Attr DrawingArea FigureState
figure = newAttr getFigure setFigure
   where getFigure o = do
                       Just f <- get o maybeFigure 
                       readMVar f 
         setFigure o f = set o [maybeFigure :~> (\(Just h) -> do
                                                              putMVar h f
                                                              return $ Just h)]
                                                     
-----------------------------------------------------------------------------

maybeFigure :: Attr DrawingArea (Maybe FigureHandle)
maybeFigure = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeFigure #-}

-----------------------------------------------------------------------------

