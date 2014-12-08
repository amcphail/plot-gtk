-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Gtk3
-- Copyright   :  (c) A. V. H. McPhail 2014
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Enables the display of 'Figure's interactively through GHCi
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Gtk3 (
                                    -- * Interface
                                    PlotHandle()
                                    , display, destroy
                                    , withPlotHandle
                                    , writePlotHandle
                                    -- * Example
                                    -- $example
                                   ) where

-----------------------------------------------------------------------------

import Control.Monad.Trans
import Control.Monad

import Control.Concurrent

import Graphics.UI.Gtk3

import Graphics.UI.Gtk3.Plot

import Graphics.Rendering.Plot

import System.IO.Unsafe

-----------------------------------------------------------------------------

data PlotHandle = PH FigureHandle (MVar DrawingArea)

-----------------------------------------------------------------------------

guiInit :: MVar Bool
{-# NOINLINE guiInit #-}
guiInit = unsafePerformIO (newMVar False)

initGUIOnce :: IO ()
initGUIOnce = do
  init_ <- readMVar guiInit 
  when (not init_) $ do
    _ <- forkOS $ runInBoundThread $ do
      _ <- unsafeInitGUIForThreadedRTS
      _ <- swapMVar guiInit True
      mainGUI --return ()
    return ()
  return ()

guiNumWindows :: MVar Int
{-# NOINLINE guiNumWindows #-}
guiNumWindows = unsafePerformIO (newMVar 0)

-----------------------------------------------------------------------------

-- | create a new figure and display the plot
--     click on the window to save
display :: Figure () -> IO PlotHandle
display f = do
   initGUIOnce
   fs <- newFigureState f
   fig <- newMVar fs
   handle <- newEmptyMVar :: IO (MVar DrawingArea)
   --
   postGUIAsync $ do
     window <- windowNew
     set window [ windowTitle := "Figure"
                , windowDefaultWidth := 400
                , windowDefaultHeight := 300
                , containerBorderWidth := 1
                ]
     --
     frame <- frameNew
     containerAdd window frame
     canvas <- plotNew fig
     containerAdd frame canvas
     --
     putMVar handle canvas
     --
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
                  fig' <- get canvas figure
                  writeFigureState ot fn s fig'
                ResponseCancel -> return ()
              widgetHide fc

     widgetShowAll window 
     --
{-
     _ <- onDestroy window $ do
                    modifyMVar_ guiNumWindows (return . (\x -> x-1))
                    nw <- readMVar guiNumWindows
                    when (nw <= 0) mainQuit
                    return ()
-}
     --
     return () --mainGUI
   return $ PH fig handle

-----------------------------------------------------------------------------

-- | close a plot
destroy :: PlotHandle -> IO ()
destroy (PH _ handle) = do
  da <- readMVar handle
  Just fr <- widgetGetParent da
  Just wi <- widgetGetParent fr
  widgetDestroy wi
  
-----------------------------------------------------------------------------

-- | perform some actions on the supplied 'PlotHandle'
withPlotHandle :: PlotHandle -> Figure () -> IO ()
withPlotHandle (PH fm cm) fig = do
                                modifyMVar_ fm $ \f -> return (updateFigureState f fig)
                                postGUIAsync $ withMVar cm (\canvas -> do
                                                        (w,h) <- widgetGetSize canvas
                                                        widgetQueueDrawArea canvas 0 0 w h) 

-- | write the 'Figure' to disk
writePlotHandle :: PlotHandle -> OutputType -> FilePath -> (Int,Int) -> IO ()
writePlotHandle (PH fm _cm) ty fn s = do
                                      fig <- readMVar fm
                                      writeFigureState ty fn s fig

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

-----------------------------------------------------------------------------

{- $example

We can create a figure:

> import Data.Colour.Names
> 
> import qualified Data.Array.IArray as A
> 
> import Numeric.Vector
> import Numeric.Matrix
> 
> import Numeric.GSL.Statistics
> 
> import Graphics.Rendering.Plot
> import Graphics.Rendering.Plot.Gtk
> 
> ln = 25
> ts = linspace ln (0,1)
> rs = randomVector 0 Gaussian ln
> 
> ss = sin (15*2*pi*ts)
> ds = 0.25*rs + ss
> es = constant (0.25*(stddev rs)) ln
> 
> fs :: Double -> Double
> fs = sin . (15*2*pi*)
> 
> ms :: Matrix Double
> ms = buildMatrix 64 64 (\(x,y) -> sin (2*2*pi*(fromIntegral x)/64) * cos (5*2*pi*(fromIntegral y)/64))
> 
> figure = do
>         withTextDefaults $ setFontFamily "OpenSymbol"
>         withTitle $ setText "Testing plot package:"
>         withSubTitle $ do
>                        setText "with 1 second of a 15Hz sine wave"
>                        setFontSize 10
>         setPlots 1 2
>         withPlot (1,1) $ do
>                          setDataset (ts,[point (ds,es,"Sampled data") (Bullet,green)
>                                        ,line (fs,"15 Hz sinusoid") blue])
>                          addAxis XAxis (Side Lower) $ do
>                                                       setGridlines Major True
>                                                       withAxisLabel $ setText "time (s)"
>                          addAxis YAxis (Side Lower) $ do
>                                                       setGridlines Major True
>                                                       withAxisLabel $ setText "amplitude"
>                          addAxis XAxis (Value 0) $ return ()
>                          setRangeFromData XAxis Lower
>                          setRange YAxis Lower (-1.25) 1.25
>                          setLegend True NorthEast Inside
>                          withLegendFormat $ setFontSize 6

observe the results:

>>> figure1 <- display figure

and then update the figure

>>> withPlotHandle figure1 $ withPlot (1,2) $ setDataset ms

and update again

>>> withPlotHandle figure1 $ withPlot (1,2) $ do { addAxis XAxis (Side Lower) $ setTickLabelFormat "%.0f" }
>>> let withfig1_12 = \d -> withPlotHandle figure1 $ withPlot (1,2) d
>>> withfig1_12 $ addAxis YAxis (Side Lower) $ setTickLabelFormat "%.0f"
>>> withfig1_12 $ setRangeFromData XAxis Lower
>>> withfig1_12 $ setRangeFromData YAxis Lower

with the multiline feature

>>> :set +m
>>> withPlotHandle figure1 $ withPlot (1,2) $ do 
>>>    addAxis XAxis (Side Lower) $ setTickLabelFormat "%.0f" 
>>>    setRangeFromData XAxis Lower
>>>    setRangeFromData YAxis Lower
-}
-----------------------------------------------------------------------------

