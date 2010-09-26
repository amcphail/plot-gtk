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
--     click on the window to save
plotNew :: FigureHandle -> IO DrawingArea
plotNew f = do
   canvas <- drawingAreaNew
   
   set canvas [maybeFigure := (Just f)]

   _ <- on canvas exposeEvent $ tryEvent $ liftIO $ do 
           s <- widgetGetSize canvas
           drw <- widgetGetDrawWindow canvas
           fig <- get canvas figure 
           renderWithDrawable drw (renderFigureState fig s)

   _ <- on canvas buttonPressEvent $ tryEvent $ liftIO $ do 
           fc <- newPlotSaveDialog
           widgetShow fc
           rsp <- dialogRun fc
           case rsp of
                    ResponseAccept -> do 
                        Just fn <- fileChooserGetFilename fc
                        Just ff <- fileChooserGetFilter fc
                        ffn <- fileFilterGetName ff
                        let ot = filterNameType ffn
                        s <- widgetGetSize canvas
                        fig <- get canvas figure
                        writeFigureState ot fn s fig
                    ResponseCancel -> return ()
           widgetHide fc

   return canvas

-----------------------------------------------------------------------------

-- | the figure attribute
figure :: Attr DrawingArea FigureState
figure = newAttr getFigure setFigure
   where getFigure o = do
                       Just f <- get o maybeFigure 
                       readMVar f 
         setFigure o f = set o [maybeFigure :~> (\(Just h) -> do
                                                              modifyMVar_ h (\_ -> return f)
                                                              return $ Just h)]
                                                     
-----------------------------------------------------------------------------

maybeFigure :: Attr DrawingArea (Maybe FigureHandle)
maybeFigure = unsafePerformIO $ objectCreateAttribute
{-# NOINLINE maybeFigure #-}

-----------------------------------------------------------------------------

newPlotSaveDialog :: IO FileChooserDialog
newPlotSaveDialog = do
                    fc <- fileChooserDialogNew (Just "Save figure") Nothing
                                         FileChooserActionSave
                                         [("Accept",ResponseAccept),("Cancel",ResponseCancel)]
                    fileChooserSetDoOverwriteConfirmation fc True
                    ff_png <- fileFilterNew
                    fileFilterSetName ff_png "PNG"
                    fileFilterAddPattern ff_png "*.png"
                    fileChooserAddFilter fc ff_png 
                    ff_ps <- fileFilterNew
                    fileFilterSetName ff_ps "PS"
                    fileFilterAddPattern ff_ps "*.ps"
                    fileChooserAddFilter fc ff_ps
                    ff_pdf <- fileFilterNew
                    fileFilterSetName ff_pdf "PDF"
                    fileFilterAddPattern ff_pdf "*.pdf"
                    fileChooserAddFilter fc ff_pdf
                    ff_svg <- fileFilterNew
                    fileFilterSetName ff_svg "SVG"
                    fileFilterAddPattern ff_png "*.svg"
                    fileChooserAddFilter fc ff_svg 
                    return fc

-----------------------------------------------------------------------------

filterNameType :: String -> OutputType
filterNameType "PNG" = PNG
filterNameType "PS"  = PS
filterNameType "PDF" = PDF
filterNameType "SVG" = SVG

{-
filenameType = filenameType' . reverse
   where filenameType' ('g':'n':'p':'.':_) = PNG
         filenameType' ('s':'p':'.':_)     = PS
         filenameType' ('f':'d':'p':'.':_) = PDF
         filenameType' ('g':'v':'s':'.':_) = SVG
         filenameType' _                   = error "Unknown file type"
-}
-----------------------------------------------------------------------------
