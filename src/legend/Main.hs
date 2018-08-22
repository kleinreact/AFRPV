-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Creates an overview pdf that explains the different visualization
-- primitives.

-----------------------------------------------------------------------------

  {-# LANGUAGE

      LambdaCase

    #-}

-----------------------------------------------------------------------------

module Main where

-----------------------------------------------------------------------------

import AFRPV.Yampa

import qualified Control.Category as C

import AFRPV.SF.Yampa
  ( components
  )

import AFRPV.SF.Observer
  ( net
  )    

import System.FilePath
  ( takeBaseName
  )

import System.IO
  ( IOMode(..)
  , writeFile
  , openFile
  )

import System.Process
  ( runProcess
  , waitForProcess
  )

import System.Directory
  ( findExecutable
  , doesFileExist
  , copyFile
  )

import Control.Monad
  ( void
  , unless
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  n <- render (C.id)
  copyFile "legend/network.tex" "legend/nId.tex"
  nId <- readTex n "nId.tex"
  
  n <- render (arr (\i -> i))
  copyFile "legend/network.tex" "legend/nPure.tex"  
  nPure <- readTex n "nPure.tex"

  n <- render (first (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nFirst.tex"    
  nFirst <- readTex n "nFirst.tex"

  n <- render (second (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nSecond.tex"      
  nSecond <- readTex n "nSecond.tex"

  n <- render ((C..) (arr (\i -> i)) (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nCompose.tex"      
  nCompose <- readTex n "nCompose.tex"

  n <- render ((arr (\i -> i)) *** (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nParallel.tex"      
  nParallel <- readTex n "nParallel.tex"

  n <- render ((arr (\i -> i)) &&& (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nFanOut.tex"      
  nFanOut <- readTex n "nFanOut.tex"

  n <- render (left (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nCLeft.tex"      
  nCLeft <- readTex n "nCLeft.tex"

  n <- render (right (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nCRight.tex"      
  nCRight <- readTex n "nCRight.tex"

  n <- render ((arr (\i -> i)) +++ (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nMerge.tex"      
  nMerge <- readTex n "nMerge.tex"

  n <- render ((arr (\i -> i)) ||| (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nFanIn.tex"      
  nFanIn <- readTex n "nFanIn.tex"

  n <- render (loop (arr (\i -> i)))
  copyFile "legend/network.tex" "legend/nLoop.tex"      
  nLoop <- readTex n "nLoop.tex"

  n <- render (initially undefined)
  copyFile "legend/network.tex" "legend/nInitially.tex"      
  nInitially <- readTex n "nInitially.tex"

  n <- render (never)
  copyFile "legend/network.tex" "legend/nSource.tex"      
  nSource <- readTex n "nSource.tex"

  n <- render (delayEvent undefined)
  copyFile "legend/network.tex" "legend/nDelay.tex"      
  nDelay <- readTex n "nDelay.tex"

  n <- render (edge)
  copyFile "legend/network.tex" "legend/nTrigger.tex"      
  nTrigger <- readTex n "nTrigger.tex"

  n <- render (hold undefined)
  copyFile "legend/network.tex" "legend/nHold.tex"      
  nHold <- readTex n "nHold.tex"

  n <- render (once)
  copyFile "legend/network.tex" "legend/nFilter.tex"      
  nFilter <- readTex n "nFilter.tex"

  n <- render (accum undefined)
  copyFile "legend/network.tex" "legend/nAccum.tex"      
  nAccum <- readTex n "nAccum.tex"

  n <- render (switch (arr (\i -> i)) undefined)
  copyFile "legend/network.tex" "legend/nSwitch.tex"      
  nSwitch <- readTex n "nSwitch.tex"  

  findExecutable "pdflatex" >>= \case
    Nothing       -> error "cannot find pdflatex executable"
    Just pdflatex -> do
      let
        texFile = "legend/legend.tex"
        outFile = "legend/legend.pdf"

        content = unlines $ 
          [ texCommand "documentclass" ["border=0pt"] ["article"]
          , texCommand "usepackage" ["top=1cm","bottom=1cm","left=1cm","right=1cm"] ["geometry"]
          , texCommand "usepackage" [] ["tikz"]            
          , texCommand "usepackage" [] ["amsmath"]
          , texCommand "usepackage" [] ["stmaryrd"]
          , texCommand "usepackage" [] ["multicol"]            
          , texCommand "usepackage" [] ["amssymb"]
          , texCommand "usetikzlibrary" []
              [ "automata"
              , "calc"
              , "shapes"
              , "arrows"
              , "shadows"
              ]
          , ""
          , "\\parindent0pt"
          , "\\pagestyle{empty}"
          , texCommand "begin" [] ["document"]
          , texCommand "begin" [] ["multicols"] ++ "{2}"
          , "\\tikzstyle{every picture}+=[inner sep=0pt]%"
          , "\\section*{Standard Arrow Operations}"
          , "\\vspace{1em}"            
          ] ++ concat
            [ entry "id ::\\ {\\color{blue}SF a a}" nId
            , entry "arr ::\\ {(\\color{blue}a} -> {\\color{blue}b}) -> {\\color{blue}SF a b}" nPure
            , entry "first ::\\ {\\color{blue}SF a b} -> {\\color{blue}SF} ({\\color{blue}a},{\\color{blue}c}) ({\\color{blue}b},{\\color{blue}c})" nFirst
            , entry "second ::\\ {\\color{blue}SF a b} -> {\\color{blue}SF} ({\\color{blue}c},{\\color{blue}a}) ({\\color{blue}c},{\\color{blue}b})" nSecond
            , entry "(.) ::\\ {\\color{blue}SF a b} -> {\\color{blue}SF b c} -> {\\color{blue}SF a c}" nCompose
            , entry "(***) ::\\ {\\color{blue}SF} ({\\color{blue}a},{\\color{blue}b}) -> {\\color{blue}SF} ({\\color{blue}a'},{\\color{blue}b'}) -> {\\color{blue}SF} ({\\color{blue}a},{\\color{blue}a'}) ({\\color{blue}b},{\\color{blue}b'})" nParallel
            , entry "(\\&\\&\\&) ::\\ {\\color{blue}SF} ({\\color{blue}a},{\\color{blue}b}) -> {\\color{blue}SF} ({\\color{blue}a},{\\color{blue}b'}) -> {\\color{blue}SF} {\\color{blue}a} ({\\color{blue}b},{\\color{blue}b'})" nFanOut
            , entry "left ::\\ {\\color{blue}SF a b} -> {\\color{blue}SF} ({\\color{blue}Either a c}) ({\\color{blue}Either b c})" nCLeft
            , entry "right ::\\ {\\color{blue}SF a b} -> {\\color{blue}SF} ({\\color{blue}Either c a}) ({\\color{blue}Either c b})" nCRight
            , entry "(+++) ::\\ {\\color{blue}SF a b} -> {\\color{blue}SF a' b'} ->  {\\color{blue}SF} ({\\color{blue}Either a a'}) ({\\color{blue}Either b b'})" nMerge
            , entry "(|||) ::\\ {\\color{blue}SF a c} -> {\\color{blue}SF b c} ->  {\\color{blue}SF} ({\\color{blue}Either a b}) {\\color{blue}c}" nFanIn
            , entry "loop ::\\ {\\color{blue}SF} ({\\color{blue}a},{\\color{blue}c}) ({\\color{blue}b},{\\color{blue}c}) -> {\\color{blue}SF a b}" nLoop
            ] ++
          [ "\\vspace{20em}"
          , "\\section*{Yampa-specific Operations}"
          , "\\vspace{1em}"  
          ] ++ concat
            [ entry "initially ::\\ {\\color{blue}a} -> {\\color{blue}SF a a}" nInitially
            , mentry "Sources"
                [ "constant ::\\ {\\color{blue}b} -> {\\color{blue}SF a b}"
                , "localTime ::\\ {\\color{blue}SF a Time}"
                , "time ::\\ {\\color{blue}SF a Time}"
                , "never ::\\ {\\color{blue}SF a} ({\\color{blue}Event b})"
                , "now ::\\ {\\color{blue}b} -> {\\color{blue}SF a} ({\\color{blue}Event b})"
                , "after ::\\ {\\color{blue}Time} -> {\\color{blue}b} -> {\\color{blue}SF a} ({\\color{blue}Event b})"
                , "repeatedly ::\\ {\\color{blue}Time} -> {\\color{blue}b} -> {\\color{blue}SF a} ({\\color{blue}Event b})"
                , "afterEach ::\\ [({\\color{blue}Time}, {\\color{blue}b})] -> {\\color{blue}SF a} ({\\color{blue}Event b})"
                , "afterEachCat ::\\ [({\\color{blue}Time}, {\\color{blue}b})] -> {\\color{blue}SF a} ({\\color{blue}Event b})"
                , "noise ::\\ ({\\color{blue}RandomGen g}, {\\color{blue}Random b}) =>  ({\\color{blue}b}, {\\color{blue}b}) -> {\\color{blue}g} -> {\\color{blue}SF a} ({\\color{blue}Event b})"
                , "noiseR ::\\ ({\\color{blue}RandomGen g}, {\\color{blue}Random b}) =>  ({\\color{blue}b}, {\\color{blue}b}) -> {\\color{blue}g} -> {\\color{blue}SF a} ({\\color{blue}Event b})"
                , "occasionally ::\\ {\\color{blue}RandomGen g} => {\\color{blue}g} -> {\\color{blue}Time} -> {\\color{blue}b} -> {\\color{blue}SF a} ({\\color{blue}Event b})"
                ] nSource
            , mentry "Delays"
                [ "delayEvent ::\\ {\\color{blue}Time} -> {\\color{blue}SF} ({\\color{blue}Event a}) ({\\color{blue}Event a})"
                , "delayEventCat ::\\ {\\color{blue}Time} -> {\\color{blue}SF} ({\\color{blue}Event a}) ({\\color{blue}Event}[{\\color{blue}a}])"
                , "pre ::\\ {\\color{blue}SF a a}"
                , "iPre ::\\ {\\color{blue}a} -> {\\color{blue}SF a a}"
                , "delay ::\\ {\\color{blue}Time} -> {\\color{blue}a} -> {\\color{blue}SF a a}"  
                ] nDelay
            , mentry "Triggers"
                [ "edge ::\\ {\\color{blue}SF Bool} ({\\color{blue}Event} ())"
                , "iEdge ::\\ {\\color{blue}Bool} -> {\\color{blue}SF Bool} ({\\color{blue}Event} ())"
                , "edgeTag ::\\ {\\color{blue}a} -> {\\color{blue}SF Bool} ({\\color{blue}Event a})"
                , "edgeJust ::\\ {\\color{blue}SF} ({\\color{blue}Maybe a}) ({\\color{blue}Event a})"
                , "edgeBy ::\\ ({\\color{blue}a} -> {\\color{blue}a} -> {\\color{blue}Maybe b}) -> {\\color{blue}a} -> {\\color{blue}SF a} ({\\color{blue}Event b})"  
                ] nTrigger
            , mentry "Holds"
                [ "hold ::\\ {\\color{blue}a} -> {\\color{blue}SF} ({\\color{blue}Event a}) {\\color{blue}a}"
                , "dHold ::\\ {\\color{blue}a} -> {\\color{blue}SF} ({\\color{blue}Event a}) {\\color{blue}a}"
                , "trackAndHold ::\\ {\\color{blue}a} -> {\\color{blue}SF} ({\\color{blue}Maybe a}) {\\color{blue}a}"
                , "accumHold ::\\ {\\color{blue}a} -> {\\color{blue}SF} ({\\color{blue}Event} ({\\color{blue}a} -> {\\color{blue}a})) {\\color{blue}a}"
                , "dAccumHold ::\\ {\\color{blue}a} -> {\\color{blue}SF} ({\\color{blue}Event} ({\\color{blue}a} -> {\\color{blue}a})) {\\color{blue}a}"
                , "accumHoldBy ::\\ ({\\color{blue}b} -> {\\color{blue}a} -> {\\color{blue}b}) -> {\\color{blue}b} -> {\\color{blue}SF} ({\\color{blue}Event a}) {\\color{blue}b}"
                , "dAccumHoldBy ::\\ ({\\color{blue}b} -> {\\color{blue}a} -> {\\color{blue}b}) -> {\\color{blue}b} -> {\\color{blue}SF} ({\\color{blue}Event a}) {\\color{blue}b}"
                ] nHold
            , mentry "Filters"
                [ "notYet ::\\ {\\color{blue}SF} ({\\color{blue}Event a}) ({\\color{blue}Event a})"
                , "once ::\\ {\\color{blue}SF} ({\\color{blue}Event a}) ({\\color{blue}Event a})"
                , "takeEvents ::\\ {\\color{blue}Int} -> {\\color{blue}SF} ({\\color{blue}Event a}) ({\\color{blue}Event a})"
                , "dropEvents ::\\ {\\color{blue}Int} -> {\\color{blue}SF} ({\\color{blue}Event a}) ({\\color{blue}Event a})"
                ] nFilter
            , mentry "Stateful Primitives"
                [ "sscan ::\\ ({\\color{blue}b} -> {\\color{blue}a} -> {\\color{blue}b}) -> {\\color{blue}b} -> {\\color{blue}SF a b}"
                , "sscanPrim ::\\ ({\\color{blue}c} -> {\\color{blue}a} -> {\\color{blue}Maybe} ({\\color{blue}c}, {\\color{blue}b})) -> {\\color{blue}c} -> {\\color{blue}b} -> {\\color{blue}SF a b}"
                , "accum ::\\ {\\color{blue}a} -> {\\color{blue}SF} ({\\color{blue}Event} ({\\color{blue}a} -> {\\color{blue}a})) ({\\color{blue}Event a})"
                , "accumBy ::\\ ({\\color{blue}b} -> {\\color{blue}a} -> {\\color{blue}b}) -> {\\color{blue}b} -> {\\color{blue}SF} ({\\color{blue}Event a}) ({\\color{blue}Event b})"
                , "accumFilter ::\\ ({\\color{blue}c} -> {\\color{blue}a} -> ({\\color{blue}c}, {\\color{blue}Maybe b})) -> {\\color{blue}c} -> {\\color{blue}SF} ({\\color{blue}Event a}) ({\\color{blue}Event b})"
                , "integral ::\\ {\\color{blue}VectorSpace a s} => {\\color{blue}SF a a}"
                , "imIntegral ::\\ {\\color{blue}VectorSpace a s} => {\\color{blue}SF a a}"
                , "impulseIntegral ::\\ {\\color{blue}VectorSpace a k} => {\\color{blue}SF} ({\\color{blue}a}, {\\color{blue}Event a}) {\\color{blue}a}"
                , "count ::\\ {\\color{blue}Integral b} => {\\color{blue}SF} ({\\color{blue}Event a}) ({\\color{blue}Event b})"
                , "derivate ::\\ {\\color{blue}VectorSpace a s} => {\\color{blue}SF a a}"
                , "iterFrom ::\\ ({\\color{blue}a} -> {\\color{blue}a} -> {\\color{blue}DTime} -> {\\color{blue}b} -> {\\color{blue}b}) -> {\\color{blue}b} -> {\\color{blue}SF a b}"
                ] nAccum
            , mentry "Switches"
                [ "switch ::\\ {\\color{blue}SF a} ({\\color{blue}b}, {\\color{blue} Event c}) -> ({\\color{blue}c} -> {\\color{blue}SF a b}) -> {\\color{blue} SF a b}"
                ] nSwitch
            ] ++
          [ texCommand "end" [] ["multicols"]            
          , texCommand "end" [] ["document"]
          ]

      writeFile texFile content
      ferror <- openFile ("legend/pdflatex.stderr") WriteMode
      fout <- openFile ("legend/pdflatex.stdout") WriteMode

      p <-
        runProcess pdflatex
          [ "-halt-on-error"
          , takeBaseName texFile
          ] (Just "legend") Nothing Nothing
            (Just fout) (Just ferror)

      void $ waitForProcess p

      fexists <- doesFileExist outFile

      unless fexists $
        error "pdflatex failed"

  where
    mentry s xs ys =
      [ s ++ ": \\\\"
      ] ++ map (\x -> "\\mbox{\\ \\ } \\scriptsize \\texttt{" ++ x ++ "} \\\\") xs ++
      [ "\\ \\\\ \\mbox{\\ } \\quad"
      ] ++ ys ++
      [ ""
      , "\\vspace{1em}"
      , ""  
      ]      
    
    entry s xs =
      [ "\\scriptsize \\texttt{" ++ s ++ "}\\\\[1em] \\mbox{\\ } \\quad"
      ] ++ xs ++
      [ ""
      , "\\vspace{1em}"
      , ""  
      ]
    
    render x = do
      renderNetwork x "legend"
      return $ length $ components $ net x

-----------------------------------------------------------------------------

readTex
  :: Int -> String -> IO [String]

readTex n file = do
  cnt <- readFile ("legend/" ++ file)
  let xs = reverse $ drop 2 $ reverse $ drop 9 $ lines cnt
  return $ xs ++
    [ "\\draw (I" ++ show (n-1) ++ ") -- +(-0.2,0);"
    , "\\draw (O" ++ show (n-1) ++ ") -- +(0.2,0);"      
    , "\\end{tikzpicture}"
    ]

-----------------------------------------------------------------------------  

texCommand
  :: String -> [String] -> [String] -> String

texCommand cmd opt args =
  "\\" ++ cmd
  ++
  (case opt of
    []   -> ""
    x:xr -> "[" ++ x ++ concatMap ((',':) . (' ':)) xr ++ "]")
  ++
  (case args of
    []   -> ""
    x:xr -> "{" ++ x ++ concatMap ((',':) . (' ':)) xr ++ "}")

-----------------------------------------------------------------------------
