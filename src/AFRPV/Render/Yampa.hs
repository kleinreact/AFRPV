-----------------------------------------------------------------------------
-- |
-- Module      :  AFRPV.Render.Yampa
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Conversion functions to render the arrow network that is created by
-- the Yampa signal function constuctors.
--
-----------------------------------------------------------------------------

  {-# LANGUAGE

      LambdaCase
    , TupleSections
    , RecordWildCards
    , ImplicitParams
    , MultiWayIf

    #-}

-----------------------------------------------------------------------------

module AFRPV.Render.Yampa
  ( renderNetwork
  ) where

-----------------------------------------------------------------------------

import AFRPV.SF.Yampa
  ( WSF
  , Network
  , YampaPrimitive(..)
  , components
  )

import AFRPV.SF.Observer
  ( ArrNet(..)
  , Observe(..)
  )

import System.FilePath
  ( (</>)
  , takeBaseName
  , takeExtensions
  , replaceExtensions
  , addExtension
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
  , createDirectoryIfMissing
  )

import Control.Monad
  ( void
  , unless
  )

import Control.Monad.ST
  ( ST
  , runST
  )

import Data.Array
  ( (!)
  )

import Data.Array.ST
  ( STArray
  , newArray
  , readArray
  , writeArray
  , runSTArray
  )

import Data.STRef
  ( STRef
  , newSTRef
  , writeSTRef
  , readSTRef
  , modifySTRef
  )

-----------------------------------------------------------------------------

-- | Renders the static network underlying the given signal function
-- as a PDF. The resulting file is created under the directory path
-- given as the second argument. If the respective directory not
-- already exists it is automatically created. For the creation of the
-- PDF, the @pdflatex@ tool together with the @tikz@ package need to
-- be present.

renderNetwork
  :: WSF a b -> FilePath -> IO ()

renderNetwork sf dir =
  findExecutable "pdflatex" >>= \case
    Nothing       -> error "cannot find pdflatex executable"
    Just pdflatex -> do
      let
        texFile = dir </> "network.tex"
        outFile = dir </> "network.pdf"

        content = unlines
          [ texCommand "documentclass" ["border=0pt"] ["standalone"]
          , texCommand "usepackage" [] ["tikz"]
          , texCommand "usepackage" [] ["amsmath"]
          , texCommand "usepackage" [] ["stmaryrd"]
          , texCommand "usepackage" [] ["amssymb"]
          , texCommand "usetikzlibrary" []
              [ "automata"
              , "calc"
              , "shapes"
              , "arrows"
              , "shadows"
              ]
          , ""
          , texCommand "begin" [] ["document"]
          , "\\tikzstyle{every picture}+=[inner sep=0pt]%"
          , texCommand "begin" [] ["tikzpicture"]
          , toTikz $ net sf 
          , texCommand "end" [] ["tikzpicture"]
          , texCommand "end" [] ["document"]
          ]

      createDirectoryIfMissing True dir
      writeFile texFile content
      ferror <- openFile (dir </> "pdflatex.stderr") WriteMode
      fout <- openFile ( dir </> "pdflatex.stdout") WriteMode

      p <-
        runProcess pdflatex
          [ "-halt-on-error"
          , takeBaseName texFile
          ] (Just dir) Nothing Nothing
            (Just fout) (Just ferror)

      void $ waitForProcess p

      fexists <- doesFileExist outFile

      unless fexists $
        error "pdflatex failed"

-----------------------------------------------------------------------------

-- | Internal data structure to the unique indices of the
-- subcomponents depending of the current components arity.

data Sub =
    None
  | Unary Int
  | Binary Int Int

-----------------------------------------------------------------------------

-- | Internal data structure used calculate the sizes of thedrawing
-- boxes for the different components.

data Component =
  Component
    { pos :: (Double, Double)
    , input :: Double
    , output :: Double
    , width :: Double
    , height :: Double
    , sub :: Sub
    }

-----------------------------------------------------------------------------

-- | Default Component

defaultC
  :: Component

defaultC =
  Component
    { pos    = (0,0)
    , input  = 0
    , output = 0
    , width  = 0
    , height = 0
    , sub    = None
    }

-----------------------------------------------------------------------------

toTikz
  :: Network String -> String

toTikz network = runST drawNet
  where
    -- default component record


    -- list of all components of the network
    xs
      :: [Network String]

    xs = components network

    -- network rendering
    drawNet
      :: ST s String

    drawNet = do
      -- first create array to have fast access to the different
      -- components via their indices
      a <- newArray (0,length xs - 1) defaultC
        :: ST s (STArray s Int Component)
      -- create counter to assign each component an individual id
      iter0 <- newSTRef (0 :: Int)
      -- pass both as implicit parameters for easier acess
      let ?a = a
      let ?iter = iter0
      -- buttom up calculation of component sizes
      sizes network
      -- top down calculation of component positions
      positions (length xs - 1) network
      -- tikz picture creation for the difffent components
      xs <- build (length xs - 1) network
      -- return the result
      return $ unlines xs

-----------------------------------------------------------------------------

-- | Buttom up calculation of component sizes.

sizes
  :: (?iter :: STRef s Int, ?a :: STArray s Int Component)
  => Network String -> ST s ()

sizes = \case
  Id -> leaf $
    let
      w = 0.0 -- width
      h = 0.35 -- height
    in
      defaultC
        { width  = w
        , height = h
        , input  = 0.5 * h
        , output = 0.5 * h
        }
  Pure -> leaf $
    let
      w = 0.3 -- width
      h = 0.35 -- height
    in
      defaultC
        { width  = w
        , height = h
        , input  = 0.5 * h
        , output = 0.5 * h
        }
  First x -> unary x $ \c0 ->
    let
      d = 0.3 -- left/right additional distance
      h = 0.2 -- additional height
    in
      defaultC
        { width  = 2 * d + width c0
        , height = h + height c0
        , input  = 0.75 * h + 0.5 * input c0
        , output = 0.75 * h + 0.5 * output c0
        }
  Second x -> unary x $ \c0 ->
    let
      d = 0.3 -- left/right additional distance
      h = 0.2 -- additional height
    in
      defaultC
        { width  = 2 * d + width c0
        , height = h + height c0
        , input  = 0.5 * height c0 + 0.25 * h + 0.5 * input c0
        , output = 0.5 * height c0 + 0.25 * h + 0.5 * output c0
        }
  Compose x y -> binary x y $ \c0 c1 ->
    let
      s  = 0.1 -- space between components

      a0 = output c0
      b0 = height c0 - output c0
      a1 = input c1
      b1 = height c1 - input c1
    in
      defaultC
        { width  = width c0 + width c1 + s
        , height = if
            | a0 >= a1 && b0 >= b1 -> height c0
            | a0 >= a1 && b0 < b1 -> a0 + b1
            | a0 <= a1 && b0 <= b1 -> height c1
            | otherwise         -> a1 + b0
        , input = if
            | a0 >= a1   -> input c0
            | otherwise -> input c0 + a1 - a0
        , output = if
            | a0 <= a1   -> output c1
            | otherwise -> output c1 + a0 - a1
        }
  Parallel x y -> binary x y $ \c0 c1 ->
    let
      d = 0.3 -- left/right additional distance
    in
      defaultC
        { width  = max (width c0) (width c1) + 2 * d
        , height = height c0 + height c1
        , input  = 0.5 * (input c0 + height c1 - input c1) + input c1
        , output = 0.5 * (output c0 + height c1 - output c1) + output c1
        }
  FanOut x y -> binary x y $ \c0 c1 ->
    let
      d = 0.3 -- left/right additional distance
    in
      defaultC
        { width  = max (width c0) (width c1) + 2 * d - 0.2
        , height = height c0 + height c1
        , input  = 0.5 * (input c0 + height c1 - input c1) + input c1
        , output = 0.5 * (output c0 + height c1 - output c1) + output c1
        }
  CLeft x -> unary x $ \c0 ->
    let
      l = 0.45 -- left additional distance
      h = 0.2 -- additional height
    in
      defaultC
        { width  = l + width c0
        , height = h + height c0
        , input  = 0.75 * h + 0.5 * input c0
        , output = 0.75 * h + 0.5 * output c0
        }
  CRight x -> unary x $ \c0 ->
    let
      l = 0.45 -- left additional distance
      h = 0.2 -- additional height
    in
      defaultC
        { width  = l + width c0
        , height = h + height c0
        , input  = 0.5 * height c0 + 0.25 * h + 0.5 * input c0
        , output = 0.5 * height c0 + 0.25 * h + 0.5 * output c0
        }
  Merge x y -> binary x y $ \c0 c1 ->
    let
      l = 0.45 -- left additional distance
    in
      defaultC
        { width  = max (width c0) (width c1) + l
        , height = height c0 + height c1
        , input  = 0.5 * (input c0 + height c1 - input c1) + input c1
        , output = 0.5 * (output c0 + height c1 - output c1) + output c1
        }
  FanIn x y -> binary x y $ \c0 c1 ->
    let
      l = 0.45 -- left additional distance
    in
      defaultC
        { width  = max (width c0) (width c1) + l
        , height = height c0 + height c1
        , input  = 0.5 * (input c0 + height c1 - input c1) + input c1
        , output = 0.5 * (output c0 + height c1 - output c1) + output c1
        }
  Loop x -> unary x $ \c0 ->
    let
      d = 0.3 -- left/right additional distance
      h = 0.2 -- additional height
    in
      defaultC
        { width  = 2 * d + width c0
        , height = h + height c0
        , input  = input c0
        , output = input c0
        }
  Other Initially -> sizes Pure
  Other (Source _) -> sizes Pure
  Other (Delay _) -> sizes Pure
  Other (Trigger _) -> sizes Pure
  Other (Hold _) -> sizes Pure
  Other (Filter _) -> sizes Pure
  Other (Accum _) -> sizes Pure
  Other (Switch _ x) -> unary x $ \c0 ->
    let
      d = 0.55 -- left/right additional distance
      h = 0.8 -- additional height
    in
      defaultC
        { width  = 2 * d + width c0
        , height = h + height c0
        , input  = h - 0.3875 + 0.5 * input c0
        , output = h - 0.3875 + 0.5 * output c0
        }
  Other (RSwitch _ _) -> error "NOT IMPLEMENTED: rSwitch is not supported yet"
  Other (KSwitch _ _ _) -> error "NOT IMPLEMENTED: kSwitch is not supported yet"
  Other _ -> error "NOT IMPLEMENTED: Switches using collections are not supported yet"

  where
    leaf
      :: (?iter :: STRef s Int, ?a :: STArray s Int Component)
      => Component -> ST s ()

    leaf c = do
      i <- readSTRef ?iter
      writeArray ?a i c
      writeSTRef ?iter $ i + 1

    unary
      :: (?iter :: STRef s Int, ?a :: STArray s Int Component)
      => Network String -> (Component -> Component) -> ST s ()

    unary x u = do
      sizes x
      ix <- (+(-1)) <$> readSTRef ?iter
      cx <- readArray ?a ix

      let i = ix + 1
      writeArray ?a i $ (u cx) { sub = Unary ix }
      writeSTRef ?iter $ i + 1

    binary
      :: (?iter :: STRef s Int, ?a :: STArray s Int Component)
      => Network String -> Network String
      -> (Component -> Component -> Component) -> ST s ()

    binary x y u = do
      sizes x
      ix <- (+(-1)) <$> readSTRef ?iter
      cx <- readArray ?a ix

      sizes y
      iy <- (+(-1)) <$> readSTRef ?iter
      cy <- readArray ?a iy

      let i = iy + 1
      writeArray ?a i $ (u cx cy) { sub = Binary ix iy }
      writeSTRef ?iter $ i + 1

-----------------------------------------------------------------------------

-- | Top down calculation of component positions

positions
  :: (?a :: STArray s Int Component)
  => Int -> Network String -> ST s ()

positions i = \case
  Id -> return ()
  Pure -> return ()
  First x ->
    let
      u c@Component{ pos = (a,b) } c0 =
        ( a
        , b + 0.5 * height c - 0.5 * height c0
        )
    in
      updUnary i x u
  Second x ->
    let
      u c@Component{ pos = (a,b) } c0 =
        ( a
        , b - 0.5 * height c + 0.5 * height c0
        )
    in
      updUnary i x u
  Compose x y  ->
    let
      ux c@Component{ pos = (a,b) } c0 c1 =
        ( a - 0.5 * width c + 0.5 * width c0
        , b - 0.5 * height c + 0.5 * height c0 +
          if output c0 >= input c1 then 0 else input c1 - output c0
        )
      uy c@Component{ pos = (a,b) } c0 c1 =
        ( a + 0.5 * width c - 0.5 * width c1
        , b - 0.5 * height c + 0.5 * height c1 +
          if input c1 >= output c0 then 0 else output c0 - input c1
        )
    in
      updBinary i x y ux uy
  Parallel x y ->
    let
      ux c@Component{ pos = (a,b) } c0 c1 =
        ( a, b + 0.5 * height c - 0.5 * height c0)
      uy c@Component{ pos = (a,b) } c0 c1 =
        ( a, b - 0.5 * height c + 0.5 * height c1)
    in
      updBinary i x y ux uy
  FanOut x y ->
    let
      ux c@Component{ pos = (a,b) } c0 c1 =
        ( a - 0.1, b + 0.5 * height c - 0.5 * height c0)
      uy c@Component{ pos = (a,b) } c0 c1 =
        ( a - 0.1, b - 0.5 * height c + 0.5 * height c1)
    in
      updBinary i x y ux uy
  CLeft x ->
    let
      u c@Component{ pos = (a,b) } c0 =
        ( a
        , b + 0.5 * height c - 0.5 * height c0
        )
    in
      updUnary i x u
  CRight x ->
    let
      u c@Component{ pos = (a,b) } c0 =
        ( a
        , b - 0.5 * height c + 0.5 * height c0
        )
    in
      updUnary i x u
  Merge x y ->
    let
      ux c@Component{ pos = (a,b) } c0 c1 =
        ( a, b + 0.5 * height c - 0.5 * height c0)
      uy c@Component{ pos = (a,b) } c0 c1 =
        ( a, b - 0.5 * height c + 0.5 * height c1)
    in
      updBinary i x y ux uy
  FanIn x y    ->
    let
      ux c@Component{ pos = (a,b) } c0 c1 =
        ( a, b + 0.5 * height c - 0.5 * height c0)
      uy c@Component{ pos = (a,b) } c0 c1 =
        ( a, b - 0.5 * height c + 0.5 * height c1)
    in
      updBinary i x y ux uy
  Loop x ->
    let
      u c@Component{ pos = (a,b) } c0 =
        ( a
        , b - 0.5 * height c + 0.5 * height c0
        )
    in
      updUnary i x u
  Other Initially -> return ()
  Other (Source _) -> return ()
  Other (Delay _) -> return ()
  Other (Trigger _) -> return ()
  Other (Hold _) -> return ()
  Other (Filter _) -> return ()
  Other (Accum _) -> return ()
  Other (Switch _ x) -> let
      u c@Component{ pos = (a,b) } c0 =
        ( a
        , b + 0.5 * height c - 0.5 * height c0 - 0.2
        )
    in
      updUnary i x u
  Other (RSwitch _ _) -> error "NOT IMPLEMENTED: rSwitch is not supported yet"
  Other (KSwitch _ _ _) -> error "NOT IMPLEMENTED: kSwitch is not supported yet"
  Other _ -> error "NOT IMPLEMENTED: Switches using collections are not supported yet"

  where
    updUnary
      :: (?a :: STArray s Int Component)
      => Int -> Network String
      -> (Component -> Component -> (Double, Double)) -> ST s ()

    updUnary i x u = do
      c@Component{ sub = Unary ix } <- readArray ?a i
      cx <- readArray ?a ix
      writeArray ?a ix cx { pos = u c cx }
      positions ix x

    updBinary
      :: (?a :: STArray s Int Component)
      => Int -> Network String -> Network String
      -> (Component -> Component -> Component -> (Double, Double))
      -> (Component -> Component -> Component -> (Double, Double))
      -> ST s ()

    updBinary i x y ux uy = do
      c@Component{ sub = Binary ix iy } <- readArray ?a i
      cx <- readArray ?a ix
      cy <- readArray ?a iy
      writeArray ?a ix cx { pos = ux c cx cy }
      writeArray ?a iy cy { pos = uy c cx cy }
      positions ix x
      positions iy y

-----------------------------------------------------------------------------

-- | Tikz picture creation for the difffent components

build
  :: (?a :: STArray s Int Component)
  => Int -> Network String -> ST s [String]

build i = \case
  Id -> do
    Component{ pos = (a,b), .. } <- readArray ?a i

    return
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width / 2) ++ "," ++ show (b - (height / 2) + input) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width / 2) ++ "," ++ show (b - (height / 2) + output) ++ ");"
      , "\\draw (I" ++ show i ++ ") -- (O" ++ show i ++ ");"
      ]
  Pure -> do
    Component{ pos = (a,b), .. } <- readArray ?a i

    return
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width / 2) ++ "," ++ show (b - (height / 2) + input) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width / 2) ++ "," ++ show (b - (height / 2) + output) ++ ");"
      , "\\node[draw, circle,minimum size=0.29cm,fill=green!20] (N" ++ show i ++ ") at ("
          ++ show a ++ ", " ++ show b ++ ") {};"
      , "\\draw (I" ++ show i ++ ") -- (N" ++ show i ++ ") -- (O" ++ show i ++ ");"
      ]
  First x -> do
    c@Component{ pos = (a,b), sub = Unary ix } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix

    return $ sx ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (B" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show (bx - 0.5 * height cx - 0.1) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.3) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.3) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , concat
          [ "\\draw"
          , " (I" ++ show ix ++ ") --"
          , " (P" ++ show i ++ ") --"
          , " (XW" ++ show i ++ ") --"
          , " (P" ++ show i ++ " |- B" ++ show i ++ ") --"
          , " (B" ++ show i ++ ") --"
          , " (B" ++ show i ++ " -| Q" ++ show i ++ ") --"
          , " (XE" ++ show i ++ ") --"
          , " (Q" ++ show i ++ ") --"
          , case x of
              Id -> " cycle;"
              _  -> " (O" ++ show ix ++ ");"
          ]
      , "\\draw (I" ++ show i ++ ") -- (XW" ++ show i ++ ");"
      , "\\draw (XE" ++ show i ++ ") -- (O" ++ show i ++ ");"
      ]
  Second x -> do
    c@Component{ pos = (a,b), sub = Unary ix } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix

    return $ sx ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (B" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show (bx + 0.5 * height cx + 0.1) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.3) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.3) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , concat
          [ "\\draw"
          , " (I" ++ show ix ++ ") --"
          , " (P" ++ show i ++ ") --"
          , " (XW" ++ show i ++ ") --"
          , " (P" ++ show i ++ " |- B" ++ show i ++ ") --"
          , " (B" ++ show i ++ ") --"
          , " (B" ++ show i ++ " -| Q" ++ show i ++ ") --"
          , " (XE" ++ show i ++ ") --"
          , " (Q" ++ show i ++ ") --"
          , case x of
              Id -> " cycle;"
              _  -> " (O" ++ show ix ++ ");"
          ]
      , "\\draw (I" ++ show i ++ ") -- (XW" ++ show i ++ ");"
      , "\\draw (XE" ++ show i ++ ") -- (O" ++ show i ++ ");"
      ]
  Compose x y  -> do
    c@Component{ pos = (a,b), sub = Binary ix iy } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix
    sy <- build iy y
    cy@Component{ pos = (ay,by) } <- readArray ?a iy

    return $ sx ++ sy ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\draw (O" ++ show ix ++ ") -- (I" ++ show iy ++");"
      ]
  Parallel x y -> do
    c@Component{ pos = (a,b), sub = Binary ix iy } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix
    sy <- build iy y
    cy@Component{ pos = (ay,by) } <- readArray ?a iy

    return $ sx ++ sy ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , "\\coordinate (R" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.1) ++ ", "
          ++ show (by - (height cy / 2) + input cy) ++ ");"
      , "\\coordinate (S" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.1) ++ ", "
          ++ show (by - (height cy / 2) + output cy) ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.3) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.3) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , case (x,y) of
          (Id, Id) ->
            concat
              [ "\\draw"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " cycle;"
              ]
          (_, Id) ->
            concat
              [ "\\draw"
              , " (I" ++ show ix ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (O" ++ show ix ++ ")"
              ]
          (Id, _) ->
            concat
              [ "\\draw"
              , " (O" ++ show iy ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (I" ++ show iy ++ ");"
              ]
          _ ->
            concat
              [ "\\draw"
              , " (I" ++ show ix ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (I" ++ show iy ++ ");"
              ] ++ "\n" ++
            concat
              [ "\\draw"
              , " (O" ++ show ix ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (O" ++ show iy ++ ");"
              ]
      , "\\draw (I" ++ show i ++ ") -- (XW" ++ show i ++ ");"
      , "\\draw (XE" ++ show i ++ ") -- (O" ++ show i ++ ");"
      ]
  FanOut x y -> do
    c@Component{ pos = (a,b), sub = Binary ix iy } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix
    sy <- build iy y
    cy@Component{ pos = (ay,by) } <- readArray ?a iy

    return $ sx ++ sy ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , "\\coordinate (R" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.1) ++ ", "
          ++ show (by - (height cy / 2) + input cy) ++ ");"
      , "\\coordinate (S" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.1) ++ ", "
          ++ show (by - (height cy / 2) + output cy) ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.1) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.3) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , case (x,y) of
          (Id, Id) ->
            concat
              [ "\\draw"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " cycle;"
              ]
          (_, Id) ->
            concat
              [ "\\draw"
              , " (I" ++ show ix ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (O" ++ show ix ++ ")"
              ]
          (Id, _) ->
            concat
              [ "\\draw"
              , " (O" ++ show iy ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (I" ++ show iy ++ ");"
              ]
          _ ->
            concat
              [ "\\draw"
              , " (I" ++ show ix ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (I" ++ show iy ++ ");"
              ] ++ "\n" ++
            concat
              [ "\\draw"
              , " (O" ++ show ix ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (O" ++ show iy ++ ");"
              ]
      , "\\draw (I" ++ show i ++ ") -- (XW" ++ show i ++ ");"
      , "\\draw (XE" ++ show i ++ ") -- (O" ++ show i ++ ");"
      , "\\node[draw,fill,circle,inner sep=0.1pt] at (XW" ++ show i ++ ") {};"
      ]
  CLeft x -> do
    c@Component{ pos = (a,b), sub = Unary ix } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix

    return $ sx ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (B" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show (bx - 0.5 * height cx - 0.1) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.15) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.15) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.15) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.15) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , concat
          [ "\\draw"
          , " (I" ++ show ix ++ ") --"
          , " (P" ++ show i ++ ") --"
          , " (XW" ++ show i ++ ") --"
          , " (P" ++ show i ++ " |- B" ++ show i ++ ") --"
          , " (B" ++ show i ++ ") --"
          , " (B" ++ show i ++ " -| Q" ++ show i ++ ") --"
          , " (XE" ++ show i ++ ") --"
          , " (Q" ++ show i ++ ") --"
          , case x of
              Id -> " cycle;"
              _  -> " (O" ++ show ix ++ ");"
          ]
      , concat
          [ "\\fill"
          , " ($ (XW" ++ show i ++ ") + (-0.09,0) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.06,0.02) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.04,0.02) $) to[out=0,in=-90]"
          , " ($ (XW" ++ show i ++ ") + (-0.02,0.04) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0,0.09) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,0.04) $) to[out=-90,in=0]"
          , " ($ (XW" ++ show i ++ ") + (0,0) $)  to[out=0,in=90]"
          , " ($ (XW" ++ show i ++ ") + (0.02,-0.04) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,-0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0,-0.09) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,-0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,-0.04) $) to[out=90,in=0]"
          , " ($ (XW" ++ show i ++ ") + (-0.04,-0.02) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.06,-0.02) $) --"
          , " cycle;"
          ]
      , concat
          [ "\\fill"
          , " ($ (XE" ++ show i ++ ") + (0.09,0) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.06,0.02) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.04,0.02) $) to[out=180,in=-90]"
          , " ($ (XE" ++ show i ++ ") + (0.02,0.04) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.02,0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (0,0.09) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.02,0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.02,0.04) $) to[out=-90,in=180]"
          , " ($ (XE" ++ show i ++ ") + (0,0) $)  to[out=180,in=90]"
          , " ($ (XE" ++ show i ++ ") + (-0.02,-0.04) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.02,-0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0,-0.09) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.02,-0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.02,-0.04) $) to[out=90,in=180]"
          , " ($ (XE" ++ show i ++ ") + (0.04,-0.02) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.06,-0.02) $) --"
          , " cycle;"
          ]
      , "\\draw (I" ++ show i ++ ") -- (XW" ++ show i ++ ");"
      , "\\draw (XE" ++ show i ++ ") -- (O" ++ show i ++ ");"
      ]
  CRight x -> do
    c@Component{ pos = (a,b), sub = Unary ix } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix

    return $ sx ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (B" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show (bx + 0.5 * height cx + 0.1) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.15) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.15) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.15) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.15) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , concat
          [ "\\draw"
          , " (I" ++ show ix ++ ") --"
          , " (P" ++ show i ++ ") --"
          , " (XW" ++ show i ++ ") --"
          , " (P" ++ show i ++ " |- B" ++ show i ++ ") --"
          , " (B" ++ show i ++ ") --"
          , " (B" ++ show i ++ " -| Q" ++ show i ++ ") --"
          , " (XE" ++ show i ++ ") --"
          , " (Q" ++ show i ++ ") --"
          , case x of
              Id -> " cycle;"
              _  -> " (O" ++ show ix ++ ");"
          ]
      , concat
          [ "\\fill"
          , " ($ (XW" ++ show i ++ ") + (-0.09,0) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.06,0.02) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.04,0.02) $) to[out=0,in=-90]"
          , " ($ (XW" ++ show i ++ ") + (-0.02,0.04) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0,0.09) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,0.04) $) to[out=-90,in=0]"
          , " ($ (XW" ++ show i ++ ") + (0,0) $)  to[out=0,in=90]"
          , " ($ (XW" ++ show i ++ ") + (0.02,-0.04) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,-0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0,-0.09) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,-0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,-0.04) $) to[out=90,in=0]"
          , " ($ (XW" ++ show i ++ ") + (-0.04,-0.02) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.06,-0.02) $) --"
          , " cycle;"
          ]
      , concat
          [ "\\fill"
          , " ($ (XE" ++ show i ++ ") + (0.09,0) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.06,0.02) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.04,0.02) $) to[out=180,in=-90]"
          , " ($ (XE" ++ show i ++ ") + (0.02,0.04) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.02,0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (0,0.09) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.02,0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.02,0.04) $) to[out=-90,in=180]"
          , " ($ (XE" ++ show i ++ ") + (0,0) $)  to[out=180,in=90]"
          , " ($ (XE" ++ show i ++ ") + (-0.02,-0.04) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.02,-0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0,-0.09) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.02,-0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.02,-0.04) $) to[out=90,in=180]"
          , " ($ (XE" ++ show i ++ ") + (0.04,-0.02) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.06,-0.02) $) --"
          , " cycle;"
          ]
      , "\\draw (I" ++ show i ++ ") -- (XW" ++ show i ++ ");"
      , "\\draw (XE" ++ show i ++ ") -- (O" ++ show i ++ ");"
      ]
  Merge x y -> do
    c@Component{ pos = (a,b), sub = Binary ix iy } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix
    sy <- build iy y
    cy@Component{ pos = (ay,by) } <- readArray ?a iy

    return $ sx ++ sy ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.15) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , "\\coordinate (R" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (by - (height cy / 2) + input cy) ++ ");"
      , "\\coordinate (S" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.15) ++ ", "
          ++ show (by - (height cy / 2) + output cy) ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.15) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (V" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (b - (height c / 2) + input c + 0.22) ++ ");"
      , "\\coordinate (W" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (b - (height c / 2) + input c - 0.25) ++ ");"
      , case (x,y) of
          (Id, Id) ->
            concat
              [ "\\draw"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " cycle;"
              ]
          (_, Id) ->
            concat
              [ "\\draw"
              , " (I" ++ show ix ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (O" ++ show ix ++ ")"
              ]
          (Id, _) ->
            concat
              [ "\\draw"
              , " (O" ++ show iy ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (I" ++ show iy ++ ");"
              ]
          _ ->
            concat
              [ "\\draw"
              , " (I" ++ show ix ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (I" ++ show iy ++ ");"
              ] ++ "\n" ++
            concat
              [ "\\draw"
              , " (O" ++ show ix ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (O" ++ show iy ++ ");"
              ]
      , "\\draw (I" ++ show i ++ ") -- (XW" ++ show i ++ ");"
      , "\\draw (XE" ++ show i ++ ") -- (O" ++ show i ++ ");"
      , concat
          [ "\\fill"
          , " ($ (XW" ++ show i ++ ") + (-0.09,0) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.06,0.02) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.04,0.02) $) to[out=0,in=-90]"
          , " ($ (XW" ++ show i ++ ") + (-0.02,0.04) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0,0.09) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,0.04) $) to[out=-90,in=0]"
          , " ($ (XW" ++ show i ++ ") + (0,0) $)  to[out=0,in=90]"
          , " ($ (XW" ++ show i ++ ") + (0.02,-0.04) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,-0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0,-0.09) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,-0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,-0.04) $) to[out=90,in=0]"
          , " ($ (XW" ++ show i ++ ") + (-0.04,-0.02) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.06,-0.02) $) --"
          , " cycle;"
          ]
      , concat
          [ "\\fill"
          , " ($ (XE" ++ show i ++ ") + (0.09,0) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.06,0.02) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.04,0.02) $) to[out=180,in=-90]"
          , " ($ (XE" ++ show i ++ ") + (0.02,0.04) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.02,0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (0,0.09) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.02,0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.02,0.04) $) to[out=-90,in=180]"
          , " ($ (XE" ++ show i ++ ") + (0,0) $)  to[out=180,in=90]"
          , " ($ (XE" ++ show i ++ ") + (-0.02,-0.04) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.02,-0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0,-0.09) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.02,-0.06) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.02,-0.04) $) to[out=90,in=180]"
          , " ($ (XE" ++ show i ++ ") + (0.04,-0.02) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.06,-0.02) $) --"
          , " cycle;"
          ]
      ]
  FanIn x y -> do
    c@Component{ pos = (a,b), sub = Binary ix iy } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix
    sy <- build iy y
    cy@Component{ pos = (ay,by) } <- readArray ?a iy

    return $ sx ++ sy ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.15) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , "\\coordinate (R" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (by - (height cy / 2) + input cy) ++ ");"
      , "\\coordinate (S" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.15) ++ ", "
          ++ show (by - (height cy / 2) + output cy) ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (max (ax + (width cx / 2)) (ay + (width cy / 2)) + 0.15) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (V" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (b - (height c / 2) + input c + 0.22) ++ ");"
      , "\\coordinate (W" ++ show i ++ ") at ("
          ++ show (min (ax - (width cx / 2)) (ay - (width cy / 2)) - 0.15) ++ ", "
          ++ show (b - (height c / 2) + input c - 0.25) ++ ");"
      , case (x,y) of
          (Id, Id) ->
            concat
              [ "\\draw"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " cycle;"
              ]
          (_, Id) ->
            concat
              [ "\\draw"
              , " (I" ++ show ix ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (O" ++ show ix ++ ")"
              ]
          (Id, _) ->
            concat
              [ "\\draw"
              , " (O" ++ show iy ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (I" ++ show iy ++ ");"
              ]
          _ ->
            concat
              [ "\\draw"
              , " (I" ++ show ix ++ ") --"
              , " (P" ++ show i ++ ") --"
              , " (XW" ++ show i ++ ") --"
              , " (R" ++ show i ++ ") --"
              , " (I" ++ show iy ++ ");"
              ] ++ "\n" ++
            concat
              [ "\\draw"
              , " (O" ++ show ix ++ ") --"
              , " (Q" ++ show i ++ ") --"
              , " (XE" ++ show i ++ ") --"
              , " (S" ++ show i ++ ") --"
              , " (O" ++ show iy ++ ");"
              ]
      , "\\draw (I" ++ show i ++ ") -- (XW" ++ show i ++ ");"
      , "\\draw (XE" ++ show i ++ ") -- (O" ++ show i ++ ");"
      , concat
          [ "\\fill"
          , " ($ (XW" ++ show i ++ ") + (-0.09,0) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.06,0.02) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.04,0.02) $) to[out=0,in=-90]"
          , " ($ (XW" ++ show i ++ ") + (-0.02,0.04) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0,0.09) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,0.04) $) to[out=-90,in=0]"
          , " ($ (XW" ++ show i ++ ") + (0,0) $)  to[out=0,in=90]"
          , " ($ (XW" ++ show i ++ ") + (0.02,-0.04) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.02,-0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (0,-0.09) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,-0.06) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.02,-0.04) $) to[out=90,in=0]"
          , " ($ (XW" ++ show i ++ ") + (-0.04,-0.02) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.06,-0.02) $) --"
          , " cycle;"
          ]
      , "\\node[draw,fill,circle,inner sep=0.1pt] at (XE" ++ show i ++ ") {};"
      ]
  Loop x -> do
    c@Component{ pos = (a,b), sub = Unary ix } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix

    return $ sx ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (B" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show (bx + 0.5 * height cx + 0.1) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.3) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.3) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2)) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2)) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , concat
          [ "\\draw"
          , " (I" ++ show ix ++ ") --"
          , " (P" ++ show i ++ ") to[out=180,in=180]"
          , " (P" ++ show i ++ " |- B" ++ show i ++ ") --"
          , " (B" ++ show i ++ " -| Q" ++ show i ++ ") to[out=0,in=0]"
          , " (Q" ++ show i ++ ") --"
          , case x of
              Id -> " cycle;"
              _  -> " (O" ++ show ix ++ ");"
          ]
      , "\\draw (P" ++ show i ++ ") -- (XW" ++ show i ++ ");"
      , "\\draw (XE" ++ show i ++ ") -- (Q" ++ show i ++ ");"
      , "\\node[minimum size=0.15cm] (N" ++ show i ++ ") at (B" ++ show i ++ ") {};"
      , concat
          [ "\\draw[fill=orange!20]"
          , " ($ (N" ++ show i ++ ".north west) + (0,-0.03) $) --"
          , " ($ (N" ++ show i ++ ".north west) + (0.03,0) $) --"
          , " ($ (N" ++ show i ++ ".north east) + (-0.03,0) $) --"
          , " ($ (N" ++ show i ++ ".north east) + (0,-0.03) $) --"
          , " ($ (N" ++ show i ++ ".south east) + (0,0.03) $) --"
          , " ($ (N" ++ show i ++ ".south east) + (-0.03,0) $) --"
          , " ($ (N" ++ show i ++ ".south west) + (0.03,0) $) --"
          , " ($ (N" ++ show i ++ ".south west) + (0,0.03) $) --"
          , " cycle;"
          ]
      ]
  Other Initially -> do
    Component{ pos = (a,b), .. } <- readArray ?a i

    return
      [ "\\coordinate (I" ++ show i ++ ") at " ++
        show (a - width / 2, b - height / 2 + input) ++ ";"
      , "\\coordinate (O" ++ show i ++ ") at " ++
        show (a + width / 2, b - height / 2 + output) ++ ";"
      , "\\node[draw, circle,minimum size=0.29cm,fill=green!20] (N" ++ show i ++ ") at " ++
        show (a, b) ++ " {};"
      , "\\node[circle,minimum size=0.1cm,fill=green!50!black,opacity=0.5] at " ++
        "(N" ++ show i ++ ") {};"
      , "\\draw (I" ++ show i ++ ") -- " ++
        "(N" ++ show i ++ ") -- " ++
        "(O" ++ show i ++ ");"
      ]
  Other (Source _) -> do
    Component{ pos = (a,b), .. } <- readArray ?a i

    return
      [ "\\coordinate (I" ++ show i ++ ") at " ++
        show (a - width / 2, b - height / 2 + input) ++ ";"
      , "\\coordinate (O" ++ show i ++ ") at " ++
        show (a + width / 2, b - height / 2 + output) ++ ";"
      , "\\node[minimum size=0.29cm] (N" ++ show i ++ ") at " ++
        show (a, b) ++ " {};"
      , "\\draw[fill=green!20] " ++
        " ($ (N" ++ show i ++ ") + (120:0.145cm) $) arc (120:-120:0.145cm) --" ++
        " cycle;"
      , "\\draw (N" ++ show i ++ ") -- " ++
        "(O" ++ show i ++ ");"
      , "\\draw ($ (I" ++ show i ++ ") + (0,0.1) $) -- " ++
        "($ (I" ++ show i ++ ") + (0,-0.1) $);"
      ]
  Other (Delay _) -> do
    Component{ pos = (a,b), .. } <- readArray ?a i

    let
      p = 0.7
      d = 0.7

      wd2 = width / 2
      hd2 = height / 2
      wd3 = width / 3

    return
      [ "\\coordinate (I" ++ show i ++ ") at " ++
        show (a - wd2, b - hd2 + input) ++ ";"
      , "\\coordinate (O" ++ show i ++ ") at " ++
        show (a + wd2, b - hd2 + output) ++ ";"
      , "\\draw[fill=yellow!20] " ++
        show (a - wd2, b + d * hd2) ++ " -- " ++
        show (a - wd2 + (1-p) * wd3, b - hd2 + input) ++ " -- " ++
        show (a - wd2, b - d * hd2) ++ " -- " ++
        show (a - wd2 + p * wd3, b - d * hd2) ++ " -- " ++
        show (a - wd2 + wd3, b - hd2 + input) ++ " -- " ++
        show (a - wd2 + p * wd3, b + d * hd2) ++ " -- " ++
        "cycle;"
      , "\\draw[fill=yellow!20] " ++
        show (a - wd2 + wd3, b + d * hd2) ++ " -- " ++
        show (a - wd2 + wd3 + (1-p) * wd3, b - (hd2) + input) ++ " -- " ++
        show (a - wd2 + wd3, b - d * hd2) ++ " -- " ++
        show (a - wd2 + wd3 + p * wd3, b - d * hd2) ++ " -- " ++
        show (a - wd2 + wd3 + wd3, b - hd2 + input) ++ " -- " ++
        show (a - wd2 + wd3 + p * wd3, b + d * hd2) ++ " -- " ++
        "cycle;"
      , "\\draw[fill=yellow!20] " ++
        show (a - wd2 + 2 * wd3, b + d * hd2) ++ " -- " ++
        show (a - wd2 + 2 * wd3 + (1-p) * wd3, b - (hd2) + input) ++ " -- " ++
        show (a - wd2 + 2 * wd3, b - d * hd2) ++ " -- " ++
        show (a - wd2 + 2 * wd3 + p * wd3, b - d * hd2) ++ " -- " ++
        show (a - wd2 + 2 * wd3 + wd3, b - hd2 + input) ++ " -- " ++
        show (a - wd2 + 2 * wd3 + p * wd3, b + d * hd2) ++ " -- " ++
        "cycle;"
      , "\\draw (I" ++ show i ++ ") -- " ++
        show (a - wd2 + (1-p) * wd3, b - hd2 + input) ++ ";"
      ]
  Other (Trigger _) -> do
    Component{ pos = (a,b), .. } <- readArray ?a i

    let
      d = 0.1
      t = 0.8

    return
      [ "\\coordinate (I" ++ show i ++ ") at " ++
        show (a - width / 2, b - height / 2 + input) ++ ";"
      , "\\coordinate (O" ++ show i ++ ") at " ++
        show (a + width / 2, b - height / 2 + output) ++ ";"
      , "\\draw[fill=violet!20] " ++
        show (a - width / 2 + d * width, b + t * height / 2) ++ " -- " ++
        show (a - width / 2 + d * width, b - t * height / 2) ++ " -- " ++
        show (a + width / 2 - d * width, b - height / 2 + output) ++ " -- " ++
        "cycle;"
      , "\\draw (I" ++ show i ++ ") -- " ++
        show (a - width / 2 + d * width, b - height / 2 + input) ++ ";"
      , "\\draw (O" ++ show i ++ ") -- " ++
        show (a + width / 2 - d * width, b - height / 2 + output) ++ ";"
      ]
  Other (Hold _) -> do
    Component{ pos = (a,b), .. } <- readArray ?a i

    return
      [ "\\coordinate (I" ++ show i ++ ") at " ++
        show (a - width / 2, b - height / 2 + input) ++ ";"
      , "\\coordinate (O" ++ show i ++ ") at " ++
        show (a + width / 2, b - height / 2 + output) ++ ";"
      , "\\node[minimum size=0.15cm] (N" ++ show i ++ ") at " ++ show (a,b) ++ " {};"
      , concat
          [ "\\draw[fill=orange!20]"
          , " ($ (N" ++ show i ++ ".north west) + (0,-0.03) $) --"
          , " ($ (N" ++ show i ++ ".north west) + (0.03,0) $) --"
          , " ($ (N" ++ show i ++ ".north east) + (-0.03,0) $) --"
          , " ($ (N" ++ show i ++ ".north east) + (0,-0.03) $) --"
          , " ($ (N" ++ show i ++ ".south east) + (0,0.03) $) --"
          , " ($ (N" ++ show i ++ ".south east) + (-0.03,0) $) --"
          , " ($ (N" ++ show i ++ ".south west) + (0.03,0) $) --"
          , " ($ (N" ++ show i ++ ".south west) + (0,0.03) $) --"
          , " cycle;"
          ]
      , "\\draw (I" ++ show i ++ ") -- " ++
        "(N" ++ show i ++ ") -- " ++
        "(O" ++ show i ++ ");"
      ]
  Other (Filter _) -> do
    Component{ pos = (a,b), .. } <- readArray ?a i

    let
      l = 0.25
      r = 0.35
      t = 0.9

    return
      [ "\\coordinate (I" ++ show i ++ ") at " ++
        show (a - width / 2, b - height / 2 + input) ++ ";"
      , "\\coordinate (O" ++ show i ++ ") at " ++
        show (a + width / 2, b - height / 2 + output) ++ ";"
      , "\\draw (I" ++ show i ++ ") -- (O" ++ show i ++ ");"
      , "\\draw[fill=cyan!20] " ++
        show (a - width / 2 + l * width, b + t * height / 2) ++ " to[bend left=15] " ++
        show (a - width / 2 + l * width, b - t * height / 2) ++ " -- " ++
        show (a + width / 2 - r * width, b - t * height / 2) ++ " to[bend right=15] " ++
        show (a + width / 2 - r * width, b + t * height / 2) ++ " -- " ++
        "cycle;"
      ]
  Other (Accum _) -> do
    Component{ pos = (a,b), .. } <- readArray ?a i

    return
      [ "\\coordinate (I" ++ show i ++ ") at " ++
        show (a - width / 2, b - height / 2 + input) ++ ";"
      , "\\coordinate (O" ++ show i ++ ") at " ++
        show (a + width / 2, b - height / 2 + output) ++ ";"
      , "\\node[draw, circle,minimum size=0.29cm,fill=green!20] (N" ++ show i ++ ") at " ++
        show (a, b) ++ " {};"
      , "\\draw[gray,thick] " ++
        " ($ (N" ++ show i ++ ") + (20:0.07cm) $) arc (20:-270:0.07cm);"
      , "\\draw[>=stealth,gray,->,ultra thin] " ++
        " ($ (N" ++ show i ++ ") + (90:0.062cm) $) -- " ++
        " ($ (N" ++ show i ++ ") + (90:0.062cm) + (0.05,0) $);"
      , "\\draw (I" ++ show i ++ ") -- " ++
        "(N" ++ show i ++ ") -- " ++
        "(O" ++ show i ++ ");"
      ]
  Other (Switch _ x) -> do
    c@Component{ pos = (a,b), sub = Unary ix } <- readArray ?a i
    sx <- build ix x
    cx@Component{ pos = (ax,bx) } <- readArray ?a ix

    return $
      [ "\\draw[fill=gray!20, rounded corners=2] " ++
        show (a - width c / 2, b + height c / 2 - 0.1) ++ " -- " ++
        show (a - width c / 2, b - height c / 2) ++ " -- " ++
        show (a + width c / 2, b - height c / 2) ++ " -- " ++
        show (a + width c / 2, b + height c / 2 - 0.1) ++ " -- " ++
        "cycle;"
      , "\\draw[rounded corners=2,fill=white] " ++
        show (ax - width cx / 2 - 0.05, bx + height cx / 2) ++ " -- " ++
        show (ax - width cx / 2 - 0.05, bx - height cx / 2) ++ " -- " ++
        show (ax + width cx / 2 + 0.05, bx - height cx / 2) ++ " -- " ++
        show (ax + width cx / 2 + 0.05, bx + height cx / 2) ++ " -- " ++
        "cycle;"
      ]
      ++ sx ++
      [ "\\coordinate (I" ++ show i ++ ") at ("
          ++ show (a - width c / 2) ++ "," ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (O" ++ show i ++ ") at ("
          ++ show (a + width c / 2) ++ "," ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (B" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show (bx - 0.5 * height cx - 0.375) ++ ");"
      , "\\coordinate (C" ++ show i ++ ") at ("
          ++ show a ++ "," ++ show b ++ ");"
      , "\\coordinate (XW" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.3) ++ ", "
          ++ show (b - (height c / 2) + input c) ++ ");"
      , "\\coordinate (XE" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.3) ++ ", "
          ++ show (b - (height c / 2) + output c) ++ ");"
      , "\\coordinate (P" ++ show i ++ ") at ("
          ++ show (ax - (width cx / 2) - 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + input cx) ++ ");"
      , "\\coordinate (Q" ++ show i ++ ") at ("
          ++ show (ax + (width cx / 2) + 0.1) ++ ", "
          ++ show (bx - (height cx / 2) + output cx) ++ ");"
      , concat
          [ "\\draw"
          , " (I" ++ show ix ++ ") --"
          , " (P" ++ show i ++ ") --"
          , " ($ (XW" ++ show i ++ ") + (0.05,0.07) $) --"
          , " ($ (XW" ++ show i ++ ") + (-0.05,0.07) $);"
          ]
      , concat
          [ "\\draw"
          , " ($ (XW" ++ show i ++ ") + (-0.05,-0.07) $) --"
          , " ($ (XW" ++ show i ++ ") + (0.05,-0.07) $) --"
          , " (P" ++ show i ++ " |- B" ++ show i ++ ") --"
          , " (B" ++ show i ++ ") --"
          , " (B" ++ show i ++ " -| Q" ++ show i ++ ") --"
          , " ($ (XE" ++ show i ++ ") + (-0.05,-0.07) $) --"
          , " ($ (XE" ++ show i ++ ") + (0.05,-0.07) $);"
          ]
      , concat
          [ "\\draw"
          , " ($ (XE" ++ show i ++ ") + (0.05,0.07) $) --"
          , " ($ (XE" ++ show i ++ ") + (-0.05,0.07) $) --"
          , " (Q" ++ show i ++ ") --"
          , case x of
              Id -> " cycle;"
              _  -> " (O" ++ show ix ++ ");"
          ]
      , "\\node[fill] at ($ (XW" ++ show i ++ ") + (-0.05,0.07) $) {};"
      , "\\node[fill] at ($ (XW" ++ show i ++ ") + (-0.05,-0.07) $) {};"
      , "\\node[fill] at ($ (XE" ++ show i ++ ") + (0.05,0.07) $) {};"
      , "\\node[fill] at ($ (XE" ++ show i ++ ") + (0.05,-0.07) $) {};"
      , "\\draw (I" ++ show i ++ ") -- ($ (XW" ++ show i ++ ") + (-0.15,0) $) --" ++
        " ($ (XW" ++ show i ++ ") + (-0.09,0.04) $);"
      , "\\draw (O" ++ show i ++ ") -- ($ (XE" ++ show i ++ ") + (0.15,0) $) --" ++
        " ($ (XE" ++ show i ++ ") + (0.09,0.04) $);"
      , "\\node[fill=blue!20,draw,minimum size=0.275cm,rounded corners=2] (N" ++ show i ++ ") at (B" ++ show i ++ ") {};"
      , "\\draw[->,line width=1] " ++
        show (ax, bx - height cx / 2) ++ " -- " ++
        "(N" ++ show i ++ ");"
      ]
  Other _ -> error "TODO"

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
