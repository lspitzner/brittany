{-# LANGUAGE OverloadedStrings #-}

module Main(main) where



import           Control.Monad

import           Data.GraphViz
import           Data.GraphViz.Types
import           Data.GraphViz.Types.Generalised
import           Data.GraphViz.Types.Monadic
import           Data.GraphViz.Printing
import           Data.GraphViz.Attributes
import           Data.GraphViz.Attributes.Complete
import qualified Data.GraphViz.Attributes.HTML as HTML
import           Data.GraphViz.Commands

import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import           Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as Text.Lazy



dataTransformationLabeled :: n -> Text -> [(n, Maybe Text)] -> [n] -> DotM n ()
dataTransformationLabeled name labelText inputs outputs = do
  node
    name
    [ shape PlainText
    , textLabel labelText
    , BgColor []
    , color White
    , Margin $ DVal 0.01
    ]
  inputs `forM_` \(i, l) -> edge
    i
    name
    ( [Dir NoDir] ++ case l of
      Nothing -> []
      Just t  -> [textLabel t]
    )
  -- edge inputs name [Dir NoDir]
  outputs `forM_` \o -> edge name o []

dataTransformation :: n -> Text -> [n] -> [n] -> DotM n ()
dataTransformation name labelText inputs outputs = do
  node
    name
    [ shape PlainText
    , textLabel labelText
    , BgColor []
    , color White
    , Margin $ DVal 0.01
    ]
  inputs `forM_` \i -> edge i name [Dir NoDir]
  outputs `forM_` \o -> edge name o []

addNote :: n -> [n] -> Text -> DotM n ()
addNote noteName refsName foo = do
  node noteName [textLabel foo, shape PlainText, color White, fontColor Gray40]
  refsName `forM_` \n -> edge n noteName [Dir NoDir, style dotted, PenWidth 1.0]

subContext :: n -> Text -> DotM n ()
subContext n t = node n [textLabel t, color Black, style solid, shape Ellipse]



main :: IO ()
main = do
  void $ runGraphviz periphery Pdf "periphery.pdf"
  void $ runGraphviz periphery Svg "periphery.svg"
  void $ runGraphviz ppm Pdf "ppm.pdf"
  void $ runGraphviz ppm Svg "ppm.svg"
  void $ runGraphviz bridocgen Pdf "bridocgen.pdf"
  void $ runGraphviz bridocgen Svg "bridocgen.svg"
 where

  periphery :: Data.GraphViz.Types.Generalised.DotGraph String
  periphery = digraph (Str ("periphery")) $ do

    -- graphAttrs [Layout Neato]
    nodeAttrs [style filled, color LightGray, shape BoxShape]
    edgeAttrs [color Gray40, PenWidth 2.0]

    cluster (Num $ Int 0) $ do
      graphAttrs [textLabel $ "input"]
      graphAttrs [color LightGray, shape Tab]
      nodeAttrs [style filled, color LightGray, shape BoxShape]
      node "stdin/input file" []
      node "program args"     []
      node "config file"      []

    cluster (Num $ Int 1) $ do
      graphAttrs [textLabel $ "output"]
      graphAttrs [color LightGray, shape Tab]
      nodeAttrs [style filled, color LightGray, shape BoxShape]
      node "output" [textLabel $ "stdout/output file"]
      node "stderr" [textLabel $ "stderr"]

    node "config" []
    "program args" --> "config"
    "config file" --> "config"
    "default config" --> "config"

    node "syntaxtree"   []
    node "annotations"  []
    node "annotations'" []

    dataTransformation "annTrans"
                       "transform slightly"
                       ["annotations"]
                       ["annotations'"]

    dataTransformation "parse"
                       "parse via ghc-exactprint"
                       ["stdin/input file"]
                       ["syntaxtree", "annotations"]

    subContext "ppmcontext" $ Text.Lazy.unlines
      [ "transformation in"
      , "PPM monadic context:"
      , "Reader: Config+Anns"
      , "Writer: Output+Errors(+Debug output)"
      ]

    "config" --> "ppmcontext"
    "annotations'" --> "ppmcontext"
    "syntaxtree" --> "ppmcontext"

    "ppmcontext" --> "output"
    "ppmcontext" --> "stderr"

  ppm :: Data.GraphViz.Types.Generalised.DotGraph String
  ppm = digraph (Str ("ppm")) $ do

    -- graphAttrs [Layout Neato]
    nodeAttrs [style filled, color LightGray, shape BoxShape]
    edgeAttrs [color Gray40, PenWidth 2.0]

    node
      "config"
      [ fontColor Gray40
      , color Gray90
      , textLabel $ Text.Lazy.unlines ["config", "(--dump-config)"]
      ]
    node
      "annotations"
      [ fontColor Gray40
      , color Gray90
      , textLabel $ Text.Lazy.unlines ["annotations", "(--dump-annotations)"]
      ]

    node "syntaxtree"
         [textLabel $ Text.Lazy.unlines ["syntaxtree", "(--dump-ast-full)"]]

    node
      "modulechildren"
      [textLabel
        $ Text.Lazy.unlines
            [ "top-level module"
            , "children, e.g."
            , "type sig, bind,"
            , "data decl etc."
            ]
      ]

    dataTransformation "syntaxSplit"
                       "split into"
                       ["syntaxtree"]
                       ["module header", "modulechildren"]

    subContext "bridocgen" $ Text.Lazy.unlines
      [ "translation into BriDoc tree"
      , "in (nested) monadic context"
      , "(additional) State: NodeAllocIndex"
      ]

    addNote "bridocgen-note"
            ["bridocgen"]
            "largest portion of\nbrittany src code"

    edge "modulechildren" "bridocgen" [textLabel $ "for each child"]
    edge "config"         "bridocgen" [color Gray70, PenWidth 1.0]
    edge "annotations"    "bridocgen" [color Gray70, PenWidth 1.0]

    node
      "bridoc-alt"
      [textLabel $ Text.Lazy.unlines
        ["BriDoc (with alternatives)", "(--dump-bridoc-raw)"]
      ]
    "bridocgen" --> "bridoc-alt"

    addNote "bridoc-alt-note"
            ["bridoc-alt"]
            "exponential size,\nbut linear using\n(explicit) sharing"

    node "spacing-info" [textLabel $ "Map from BriDoc node\nto spacing info"]
    dataTransformation "getSpacing"
                       "getSpacing/getSpacings"
                       ["bridoc-alt"]
                       ["spacing-info"]

    addNote "spacing-info-note" ["spacing-info"] $ Text.Lazy.unlines
      [ "roughly: how much cols/rows"
      , "each Bridoc subtree takes"
      , "in bottom-up fashion"
      ]

    node    "bridoc-no-alt"     [textLabel $ "BriDoc without Alts"]
    dataTransformation "transformAlts"
                       "transformAlts"
                       ["bridoc-alt", "spacing-info"]
                       ["bridoc-no-alt"]

    edge "config" "transformAlts" [color Gray70, PenWidth 1.0]

    addNote "bridocnoaltnote" ["spacing-info", "bridoc-no-alt"] "linear size"

    edge "bridocnoaltnote"
         "bridoc-final"
         [Dir NoDir, style dotted, PenWidth 1.0]

    node
      "bridoc-final"
      [textLabel $ Text.Lazy.unlines
        ["transformed BriDoc", "(--dump-bridoc-final)"]
      ]
    dataTransformation "otherTransforms"
                       "transformFloating\ntransformColumn\ntransformPar"
                       ["bridoc-no-alt"]
                       ["bridoc-final"]

    addNote "otherTransforms-note"
            ["transformAlts", "otherTransforms"]
            "most cpu/memory\nusage happens here"

    "bridoc-final" --> "backend"
    edge "annotations" "backend" [color Gray70, PenWidth 1.0]
    edge "config"      "backend" [color Gray70, PenWidth 1.0]

    subContext "backend" $ Text.Lazy.unlines
      [ "backend:"
      , "BriDoc -> Text 'rendering'"
      , "in (nested) monadic context"
      , "(additional) State: LayoutState"
      ]

    addNote "backendnote" ["backend"] $ Text.Lazy.unlines
      ["'LayoutState' really is", "just the state for", "the backend only."]

    "backend" --> "output text fragments"

    dataTransformationLabeled
      "outputConcat"
      "output concatenation"
      [("output text fragments", Nothing), ("module header", Just $ "as-is")]
      ["output text"]

  bridocgen :: Data.GraphViz.Types.Generalised.DotGraph String
  bridocgen = digraph (Str ("bridocgen")) $ do

    -- graphAttrs [Layout Neato]
    nodeAttrs [style filled, color LightGray, shape BoxShape]
    edgeAttrs [color Gray40, PenWidth 2.0]

    node "type of node?" [shape DiamondShape, style solid, color Black]

    edge "top-level module children" "type of node?" []

    dataTransformationLabeled "layoutSig"
                              "layoutSig\n+recursion\n(layoutType etc.)"
                              [("type of node?", Just "type sig")]
                              ["BriDoc (tree)"]

    dataTransformationLabeled "layoutBind"
                              "layoutBind\n+recursion\n(layoutExpr etc.)"
                              [("type of node?", Just "equation")]
                              ["BriDoc (tree)"]

    dataTransformationLabeled "layoutByExact"
                              "layoutByExact"
                              [("type of node?", Just "not handled (yet)")]
                              ["BriDoc (tree)"]

  -- backend :: Data.GraphViz.Types.Generalised.DotGraph String
  -- backend = digraph (Str ("ppm")) $ do

