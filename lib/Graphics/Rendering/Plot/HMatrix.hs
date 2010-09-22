-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.HMatrix
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Compatability module to replace "Graphics.Plot" of the 'hmatrix' module
--
-- Provides all functions from hmatrix's "Graphics.Plot" as well as 
-- those functions appended with 'H' which return a 'PlotHandle' for
-- interactive update.
--
-- Comments and function signatures were copied from "hmatrix:Graphics.Plot"
-- which is released under a GPL-like licence
--
-- gnuplot*, splot, mesh, and mesh' are copied from "hmatrix:Graphics.Plot"
-- (c) A. Ruiz
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.HMatrix (
                                        -- * Plotting functions
                                        mplot, mplotH
                                       , plot, plotH
                                       , parametricPlot, parametricPlotH
                                       , imshow, greyscaleH
                                       , meshdom
                                        -- * Compatability
                                       , matrixToPGM
                                       , gnuplotX, gnuplotpdf, gnuplotWin
                                        -- * Undefined functions
                                       , splot, mesh, mesh'
                                       ) where

-----------------------------------------------------------------------------

import Numeric.Vector
import Numeric.Matrix

{- COMPATABILITY -} 
import Data.List(intersperse)
import System.Process (system)

import Graphics.Rendering.Plot.Figure

import qualified Graphics.Rendering.Plot.Figure.Simple as S

import Graphics.Rendering.Plot.Gtk

-----------------------------------------------------------------------------

nohandle m = m >> return ()

-----------------------------------------------------------------------------

-- | plot several vectors against the first
mplot :: [Vector Double] -> IO ()
mplot = nohandle . mplotH

-- | plot several vectors against the first
mplotH :: [Vector Double] -> IO PlotHandle
mplotH [] = error "mplot': no data"
mplotH [_] = error "mplot': no ordinates"
mplotH (v:vs) = display $ S.plot (Line,v,vs)

-----------------------------------------------------------------------------

-- apply several functions to one object
mapf fs x = map ($ x) fs

{- | Draws a list of functions over a desired range and with a desired number of points 

> > plot [sin, cos, sin.(3*)] (0,2*pi) 1000

-}
plot :: [Vector Double -> Vector Double] -> (Double,Double) -> Int -> IO ()
plot fs r n = nohandle $ plotH fs r n

{- | Draws a list of functions over a desired range and with a desired number of points 

> > plot [sin, cos, sin.(3*)] (0,2*pi) 1000

-}
plotH :: [Vector Double -> Vector Double] -> (Double,Double) -> Int -> IO PlotHandle
plotH fs r n = display $ do
                         let ts = linspace n r
                         S.plot (Line,ts,mapf fs ts)

-----------------------------------------------------------------------------

{- | Draws a parametric curve. For instance, to draw a spiral we can do something like:

> > parametricPlot (\t->(t * sin t, t * cos t)) (0,10*pi) 1000

-}
parametricPlot :: (Vector Double->(Vector Double,Vector Double)) -> (Double, Double) -> Int -> IO ()
parametricPlot f r n = nohandle $ parametricPlotH f r n

{- | Draws a parametric curve. For instance, to draw a spiral we can do something like:

> > parametricPlot (\t->(t * sin t, t * cos t)) (0,10*pi) 1000

-}
parametricPlotH :: (Vector Double->(Vector Double,Vector Double)) -> (Double, Double) -> Int -> IO PlotHandle
parametricPlotH f r n = display $ do
                                  let t = linspace n r
                                      (fx,fy) = f t
                                  S.plot (Line,fx,[fy])

-----------------------------------------------------------------------------

-- | From vectors x and y, it generates a pair of matrices to be used as x and y arguments for matrix functions.
meshdom :: Vector Double -> Vector Double -> (Matrix Double , Matrix Double)
meshdom r1 r2 = (outer r1 (constant 1 (dim r2)), outer (constant 1 (dim r1)) r2)

gnuplotX :: String -> IO ()
gnuplotX command = do { _ <- system cmdstr; return()} where
    cmdstr = "echo \""++command++"\" | gnuplot -persist"

datafollows = "\\\"-\\\""

prep = (++"e\n\n") . unlines . map (unwords . (map show))

{- | Draws a 3D surface representation of a real matrix.

> > mesh (hilb 20)

In certain versions you can interactively rotate the graphic using the mouse.

UNDEFINED: (c) A. Ruiz
-}
mesh :: Matrix Double -> IO ()
mesh m = gnuplotX (command++dat) where
    command = "splot "++datafollows++" matrix with lines\n"
    dat = prep $ toLists $ m

mesh' :: Matrix Double -> IO ()
mesh' m = do
    writeFile "splot-gnu-command" "splot \"splot-tmp.txt\" matrix with lines; pause -1"; 
    toFile' "splot-tmp.txt" m
    putStr "Press [Return] to close the graphic and continue... "
    _ <- system "gnuplot -persist splot-gnu-command"
    _ <- system "rm splot-tmp.txt splot-gnu-command"
    return ()

{- | Draws the surface represented by the function f in the desired ranges and number of points, internally using 'mesh'.

> > let f x y = cos (x + y) 
> > splot f (0,pi) (0,2*pi) 50    

UNDEFINED: (c) A.Ruiz

-}
splot :: (Matrix Double->Matrix Double->Matrix Double) -> (Double,Double) -> (Double,Double) -> Int -> IO () 
splot f rx ry n = mesh' z where
    (x,y) = meshdom (linspace n rx) (linspace n ry)
    z = f x y

-----------------------------------------------------------------------------

-- | writes a matrix to pgm image file
--     (c) A. Ruiz, hmatrix package
matrixToPGM :: Matrix Double -> String
matrixToPGM m = header ++ unlines (map unwords ll) where
    c = cols m
    r = rows m
    header = "P2 "++show c++" "++show r++" "++show (round maxgray :: Int)++"\n"
    maxgray = 255.0
    maxval = maxElement m
    minval = minElement m
    scale' = if (maxval == minval) 
        then 0.0
        else maxgray / (maxval - minval)
    f x = show ( round ( scale' *(x - minval) ) :: Int )
    ll = map (map f) (toLists m)

-----------------------------------------------------------------------------

-- | imshow shows a representation of a matrix as a gray level image.
imshow :: Matrix Double -> IO ()
imshow = nohandle . greyscaleH

-- | greyscaleH shows a representation of a matrix as a gray level image.
greyscaleH :: Matrix Double -> IO PlotHandle
greyscaleH d = display $ S.plot d

-----------------------------------------------------------------------------

-- | Saves a real matrix to a formatted ascii text file
toFile' :: FilePath -> Matrix Double -> IO ()
toFile' filename matrix = writeFile filename (unlines . map unwords. map (map show) . toLists $ matrix)

gnuplotpdf :: String -> String -> [([[Double]], String)] -> IO ()
gnuplotpdf title command ds = gnuplot (prelude ++ command ++" "++ draw) >> postproc where
    prelude = "set terminal epslatex color; set output '"++title++".tex';"
    (dats,defs) = unzip ds
    draw = concat (intersperse ", " (map ("\"-\" "++) defs)) ++ "\n" ++
           concatMap pr dats
    postproc = do
        _ <- system $ "epstopdf "++title++".eps"
        mklatex
        _ <- system $ "pdflatex "++title++"aux.tex > /dev/null"
        _ <- system $ "pdfcrop "++title++"aux.pdf > /dev/null"
        _ <- system $ "mv "++title++"aux-crop.pdf "++title++".pdf"
        _ <- system $ "rm "++title++"aux.* "++title++".eps "++title++".tex"
        return ()

    mklatex = writeFile (title++"aux.tex") $
       "\\documentclass{article}\n"++
       "\\usepackage{graphics}\n"++
       "\\usepackage{nopageno}\n"++
       "\\usepackage{txfonts}\n"++
       "\\renewcommand{\\familydefault}{phv}\n"++
       "\\usepackage[usenames]{color}\n"++

       "\\begin{document}\n"++

       "\\begin{center}\n"++
       "  \\input{./"++title++".tex}\n"++
       "\\end{center}\n"++

       "\\end{document}"

    pr = (++"e\n") . unlines . map (unwords . (map show))

    gnuplot cmd = do
        writeFile "gnuplotcommand" cmd
        _ <- system "gnuplot gnuplotcommand"
        _ <- system "rm gnuplotcommand"
        return ()

gnuplotWin :: String -> String -> [([[Double]], String)] -> IO ()
gnuplotWin title command ds = gnuplot (prelude ++ command ++" "++ draw) where
    (dats,defs) = unzip ds
    draw = concat (intersperse ", " (map ("\"-\" "++) defs)) ++ "\n" ++
           concatMap pr dats

    pr = (++"e\n") . unlines . map (unwords . (map show))

    prelude = "set title \""++title++"\";"

    gnuplot cmd = do
        writeFile "gnuplotcommand" cmd
        _ <- system "gnuplot -persist gnuplotcommand"
        _ <- system "rm gnuplotcommand"
        return ()

-----------------------------------------------------------------------------
