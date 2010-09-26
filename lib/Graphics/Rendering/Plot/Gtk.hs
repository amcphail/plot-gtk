-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Gtk
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Enables the display of 'Figure's interactively through GHCi
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Gtk (
                                    -- * Interface
                                    PlotHandle()
                                    , display
                                    , withPlotHandle
                                    , writePlotHandle
                                    -- * Example
                                    -- $example
                                   ) where

-----------------------------------------------------------------------------

import Control.Concurrent

import Graphics.UI.Gtk

import Graphics.UI.Gtk.Plot

import Graphics.Rendering.Plot

-----------------------------------------------------------------------------

data PlotHandle = PH FigureHandle (MVar DrawingArea)

-----------------------------------------------------------------------------

-- | create a new figure and display the plot
--     click on the window to save
display :: Figure () -> IO PlotHandle
display f = do
   fs <- newFigureState f
   fig <- newMVar fs
   handle <- newEmptyMVar :: IO (MVar DrawingArea)
   _ <- forkOS $ runInBoundThread $ do
                 _ <- initGUI       -- is start
                 --
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
                 widgetShowAll window 
                 --
                 _ <- onDestroy window mainQuit
                 --
                 mainGUI
   return $ PH fig handle


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

-}
-----------------------------------------------------------------------------

