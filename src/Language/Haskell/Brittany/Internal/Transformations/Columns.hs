module Language.Haskell.Brittany.Internal.Transformations.Columns
  ( transformSimplifyColumns
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Utils
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Types

import qualified Data.Generics.Uniplate.Direct as Uniplate



transformSimplifyColumns :: BriDoc -> BriDoc
transformSimplifyColumns = Uniplate.rewrite $ \case
  -- BDWrapAnnKey annKey bd ->
  --   BDWrapAnnKey annKey $ transformSimplify bd
  BDEmpty -> Nothing
  BDLit{} -> Nothing
  BDSeq list | any (\case BDSeq{} -> True
                          BDEmpty{} -> True
                          _ -> False) list -> Just $ BDSeq $
    filter isNotEmpty list >>= \case
      BDSeq l -> l
      x -> [x]
  BDSeq (BDCols sig1 cols1@(_:_):rest) ->
    Just $ BDCols sig1 (List.init cols1 ++ [BDSeq (List.last cols1:rest)])
  BDLines lines | any (\case BDLines{} -> True
                             BDEmpty{} -> True
                             _ -> False) lines ->
    Just $ BDLines $ filter isNotEmpty $ lines >>= \case
      BDLines l -> l
      x -> [x]
  -- prior floating in
  BDAnnotationPrior annKey1 (BDSeq (l:lr)) ->
    Just $ BDSeq (BDAnnotationPrior annKey1 l:lr)
  BDAnnotationPrior annKey1 (BDLines (l:lr)) ->
    Just $ BDLines (BDAnnotationPrior annKey1 l:lr)
  BDAnnotationPrior annKey1 (BDCols sig (l:lr)) ->
    Just $ BDCols sig (BDAnnotationPrior annKey1 l:lr)
  -- post floating in
  BDAnnotationRest annKey1 (BDSeq list) ->
    Just $ BDSeq $ List.init list ++ [BDAnnotationRest annKey1 $ List.last list]
  BDAnnotationRest annKey1 (BDLines list) ->
    Just $ BDLines $ List.init list ++ [BDAnnotationRest annKey1 $ List.last list]
  BDAnnotationRest annKey1 (BDCols sig cols) ->
    Just $ BDCols sig $ List.init cols ++ [BDAnnotationRest annKey1 $ List.last cols]
  BDAnnotationKW annKey1 kw (BDSeq list) ->
    Just $ BDSeq $ List.init list ++ [BDAnnotationKW annKey1 kw $ List.last list]
  BDAnnotationKW annKey1 kw (BDLines list) ->
    Just $ BDLines $ List.init list ++ [BDAnnotationKW annKey1 kw $ List.last list]
  BDAnnotationKW annKey1 kw (BDCols sig cols) ->
    Just $ BDCols sig $ List.init cols ++ [BDAnnotationKW annKey1 kw $ List.last cols]
  -- ensureIndent float-in
  -- not sure if the following rule is necessary; tests currently are
  -- unaffected.
  -- BDEnsureIndent indent (BDLines lines) ->
  --   Just $ BDLines $ BDEnsureIndent indent <$> lines
  -- matching col special transformation
  BDCols sig1 cols1@(_:_)
    | BDLines lines@(_:_:_) <- List.last cols1
    , BDCols sig2 cols2 <- List.last lines
    , sig1==sig2 ->
        Just $ BDLines
          [ BDCols sig1 $ List.init cols1 ++ [BDLines $ List.init lines]
          , BDCols sig2 cols2
          ]
  BDCols sig1 cols1@(_:_)
    | BDLines lines@(_:_:_) <- List.last cols1
    , BDEnsureIndent _ (BDCols sig2 cols2) <- List.last lines
    , sig1==sig2 ->
        Just $ BDLines
          [ BDCols sig1 $ List.init cols1 ++ [BDLines $ List.init lines]
          , BDCols sig2 cols2
          ]
  BDPar ind col1@(BDCols sig1 _) col2@(BDCols sig2 _) | sig1==sig2 ->
    Just $ BDAddBaseY ind (BDLines [col1, col2])
  BDPar ind col1@(BDCols sig1 _) (BDLines (col2@(BDCols sig2 _):rest))
    | sig1==sig2 ->
    Just $ BDPar ind (BDLines [col1, col2]) (BDLines rest)
  BDPar ind (BDLines lines1) col2@(BDCols sig2 _)
    | BDCols sig1 _ <- List.last lines1
    , sig1==sig2 ->
    Just $ BDAddBaseY ind (BDLines $ lines1 ++ [col2])
  BDPar ind (BDLines lines1) (BDLines (col2@(BDCols sig2 _):rest))
    | BDCols sig1 _ <- List.last lines1
    , sig1==sig2 ->
    Just $ BDPar ind (BDLines $ lines1 ++ [col2]) (BDLines rest)
  -- BDPar ind1 (BDCols sig1 cols1) (BDPar ind2 line (BDCols sig2 cols2))
  --   | sig1==sig2 ->
  --       Just $ BDPar
  --         ind1
  --         (BDLines [BDCols sig1 cols1, BDCols sig])
  BDCols sig1 cols | BDPar _ind line (BDCols sig2 cols2) <- List.last cols
                   , sig1==sig2 ->
    Just $ BDLines
      [ BDCols sig1 (List.init cols ++ [line])
      , BDCols sig2 cols2
      ]
  BDCols sig1 cols | BDPar ind line (BDLines lines) <- List.last cols
                   , BDCols sig2 cols2 <- List.last lines
                   , sig1==sig2 ->
    Just $ BDLines
      [ BDCols sig1 $ List.init cols ++ [BDPar ind line (BDLines $ List.init lines)]
      , BDCols sig2 cols2
      ]
  BDLines [x]         -> Just $ x
  BDLines []          -> Just $ BDEmpty
  BDSeq{}             -> Nothing
  BDCols{}            -> Nothing
  BDSeparator         -> Nothing
  BDAddBaseY{}        -> Nothing
  BDBaseYPushCur{}    -> Nothing
  BDBaseYPop{}        -> Nothing
  BDIndentLevelPushCur{} -> Nothing
  BDIndentLevelPop{}  -> Nothing
  BDPar{}             -> Nothing
  BDAlt{}             -> Nothing
  BDForceMultiline{}  -> Nothing
  BDForceSingleline{} -> Nothing
  BDForwardLineMode{} -> Nothing
  BDExternal{}        -> Nothing
  BDLines{}           -> Nothing
  BDAnnotationPrior{} -> Nothing
  BDAnnotationKW{}    -> Nothing
  BDAnnotationRest{}  -> Nothing
  BDEnsureIndent{}    -> Nothing
  BDProhibitMTEL{}    -> Nothing
  BDSetParSpacing{}   -> Nothing
  BDForceParSpacing{} -> Nothing
  BDDebug{}           -> Nothing
  BDNonBottomSpacing x -> Just x
