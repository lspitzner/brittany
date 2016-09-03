module Language.Haskell.Brittany.Transformations.Floating
  ( transformSimplifyFloating
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Utils
import           Language.Haskell.Brittany.Config.Types
import           Language.Haskell.Brittany.Types

import qualified Data.Generics.Uniplate.Direct as Uniplate



-- note that this is not total, and cannot be with that exact signature.
mergeIndents :: BrIndent -> BrIndent -> BrIndent
mergeIndents BrIndentNone        x                   = x
mergeIndents x                   BrIndentNone        = x
mergeIndents (BrIndentSpecial i) (BrIndentSpecial j) = BrIndentSpecial (max i j)
mergeIndents _                   _                   = error "mergeIndents"


transformSimplifyFloating :: BriDoc -> BriDoc
transformSimplifyFloating = stepBO .> stepFull
  -- note that semantically, stepFull is completely sufficient.
  -- but the bottom-up switch-to-top-down-on-match transformation has much
  -- better complexity.
  -- UPDATE: by now, stepBO does more than stepFull; for semantic equivalence
  --         the push/pop cases would need to be copied over
  where
    descendPrior = transformDownMay $ \case
      -- prior floating in
      BDAnnotationPrior annKey1 (BDPar ind line indented) ->
         Just $ BDPar ind (BDAnnotationPrior annKey1 line) indented
      BDAnnotationPrior annKey1 (BDSeq (l:lr)) ->
         Just $ BDSeq (BDAnnotationPrior annKey1 l:lr)
      BDAnnotationPrior annKey1 (BDLines (l:lr)) ->
         Just $ BDLines (BDAnnotationPrior annKey1 l:lr)
      BDAnnotationPrior annKey1 (BDCols sig (l:lr)) ->
         Just $ BDCols sig (BDAnnotationPrior annKey1 l:lr)
      BDAnnotationPrior annKey1 (BDAddBaseY indent x) ->
         Just $ BDAddBaseY indent $ BDAnnotationPrior annKey1 x
      BDAnnotationPrior annKey1 (BDDebug s x) ->
         Just $ BDDebug s $ BDAnnotationPrior annKey1 x
      _ -> Nothing
    descendRest = transformDownMay $ \case
      -- post floating in
      BDAnnotationRest annKey1 (BDPar ind line indented) ->
        Just $ BDPar ind line $ BDAnnotationRest annKey1 indented
      BDAnnotationRest annKey1 (BDSeq list) ->
        Just $ BDSeq $ List.init list ++ [BDAnnotationRest annKey1 $ List.last list]
      BDAnnotationRest annKey1 (BDLines list) ->
        Just $ BDLines $ List.init list ++ [BDAnnotationRest annKey1 $ List.last list]
      BDAnnotationRest annKey1 (BDCols sig cols) ->
        Just $ BDCols sig $ List.init cols ++ [BDAnnotationRest annKey1 $ List.last cols]
      BDAnnotationRest annKey1 (BDAddBaseY indent x) ->
        Just $ BDAddBaseY indent $ BDAnnotationRest annKey1 x
      BDAnnotationRest annKey1 (BDDebug s x) ->
        Just $ BDDebug s $ BDAnnotationRest annKey1 x
      _ -> Nothing
    descendKW = transformDownMay $ \case
      -- post floating in
      BDAnnotationKW annKey1 kw (BDPar ind line indented) ->
        Just $ BDPar ind line $ BDAnnotationKW annKey1 kw indented
      BDAnnotationKW annKey1 kw (BDSeq list) ->
        Just $ BDSeq $ List.init list ++ [BDAnnotationKW annKey1 kw $ List.last list]
      BDAnnotationKW annKey1 kw (BDLines list) ->
        Just $ BDLines $ List.init list ++ [BDAnnotationKW annKey1 kw $ List.last list]
      BDAnnotationKW annKey1 kw (BDCols sig cols) ->
        Just $ BDCols sig $ List.init cols ++ [BDAnnotationKW annKey1 kw $ List.last cols]
      BDAnnotationKW annKey1 kw (BDAddBaseY indent x) ->
        Just $ BDAddBaseY indent $ BDAnnotationKW annKey1 kw x
      BDAnnotationKW annKey1 kw (BDDebug s x) ->
        Just $ BDDebug s $ BDAnnotationKW annKey1 kw x
      _ -> Nothing
    descendBYPush = transformDownMay $ \case
      BDBaseYPushCur (BDCols sig cols@(_:_)) ->
        Just $ BDCols sig (BDBaseYPushCur (List.head cols) : List.tail cols)
      BDBaseYPushCur (BDDebug s x) ->
        Just $ BDDebug s (BDBaseYPushCur x)
      _ -> Nothing
    descendBYPop = transformDownMay $ \case
      BDBaseYPop (BDCols sig cols@(_:_)) ->
        Just $ BDCols sig (List.init cols ++ [BDBaseYPop (List.last cols)])
      BDBaseYPop (BDDebug s x) ->
        Just $ BDDebug s (BDBaseYPop x)
      _ -> Nothing
    descendILPush = transformDownMay $ \case
      BDIndentLevelPushCur (BDCols sig cols@(_:_)) ->
        Just $ BDCols sig (BDIndentLevelPushCur (List.head cols) : List.tail cols)
      BDIndentLevelPushCur (BDDebug s x) ->
        Just $ BDDebug s (BDIndentLevelPushCur x)
      _ -> Nothing
    descendILPop = transformDownMay $ \case
      BDIndentLevelPop (BDCols sig cols@(_:_)) ->
        Just $ BDCols sig (List.init cols ++ [BDIndentLevelPop (List.last cols)])
      BDIndentLevelPop (BDDebug s x) ->
        Just $ BDDebug s (BDIndentLevelPop x)
      _ -> Nothing
    descendAddB = transformDownMay $ \case
      -- AddIndent floats into Lines.
      BDAddBaseY BrIndentNone x ->
        Just x
      BDAddBaseY indent (BDLines lines) ->
        Just $ BDLines $ BDAddBaseY indent <$> lines
      -- AddIndent floats into last column
      BDAddBaseY indent (BDCols sig cols) ->
        Just $ BDCols sig $ List.init cols ++ [BDAddBaseY indent $ List.last cols]
      -- merge AddIndent and Par
      BDAddBaseY ind1 (BDPar ind2 line indented) ->
        Just $ BDPar (mergeIndents ind1 ind2) line indented
      BDAddBaseY ind (BDAnnotationPrior annKey1 x) ->
        Just $ BDAnnotationPrior annKey1 (BDAddBaseY ind x)
      BDAddBaseY ind (BDAnnotationRest annKey1 x) ->
        Just $ BDAnnotationRest annKey1 (BDAddBaseY ind x)
      BDAddBaseY ind (BDAnnotationKW annKey1 kw x) ->
        Just $ BDAnnotationKW annKey1 kw (BDAddBaseY ind x)
      BDAddBaseY ind (BDSeq list) ->
        Just $ BDSeq $ List.init list ++ [BDAddBaseY ind (List.last list)]
      BDAddBaseY _ lit@BDLit{} ->
        Just $ lit
      BDAddBaseY ind (BDBaseYPushCur x) ->
        Just $ BDBaseYPushCur (BDAddBaseY ind x)
      BDAddBaseY ind (BDBaseYPop x) ->
        Just $ BDBaseYPop (BDAddBaseY ind x)
      BDAddBaseY ind (BDDebug s x) ->
        Just $ BDDebug s (BDAddBaseY ind x)
      _ -> Nothing
    stepBO :: BriDoc -> BriDoc
    stepBO = -- traceFunctionWith "stepBO" (show . briDocToDocWithAnns) (show . briDocToDocWithAnns) $
             transformUp f
      where
        f = \case
          x@BDAnnotationPrior{}    -> descendPrior x
          x@BDAnnotationKW{}       -> descendKW x
          x@BDAnnotationRest{}     -> descendRest  x
          x@BDAddBaseY{}           -> descendAddB  x
          x@BDBaseYPushCur{}       -> descendBYPush x
          x@BDBaseYPop{}           -> descendBYPop x
          x@BDIndentLevelPushCur{} -> descendILPush x
          x@BDIndentLevelPop{}     -> descendILPop x
          x -> x
    stepFull = -- traceFunctionWith "stepFull" (show . briDocToDocWithAnns) (show . briDocToDocWithAnns) $
               Uniplate.rewrite $ \case
      -- AddIndent floats into Lines.
      BDAddBaseY BrIndentNone x ->
        Just $ x
      BDAddBaseY indent (BDLines lines) ->
        Just $ BDLines $ BDAddBaseY indent <$> lines
      -- AddIndent floats into last column
      BDAddBaseY indent (BDCols sig cols) ->
        Just $ BDCols sig $ List.init cols ++ [BDAddBaseY indent $ List.last cols]
      BDAddBaseY ind (BDSeq list) ->
        Just $ BDSeq $ List.init list ++ [BDAddBaseY ind (List.last list)]
      -- merge AddIndent and Par
      BDAddBaseY ind1 (BDPar ind2 line indented) ->
        Just $ BDPar (mergeIndents ind1 ind2) line indented
      BDAddBaseY _ lit@BDLit{} ->
        Just $ lit
      BDAddBaseY ind (BDBaseYPushCur x) ->
        Just $ BDBaseYPushCur (BDAddBaseY ind x)
      BDAddBaseY ind (BDBaseYPop x) ->
        Just $ BDBaseYPop (BDAddBaseY ind x)
      -- prior floating in
      BDAnnotationPrior annKey1 (BDPar ind line indented) ->
        Just $ BDPar ind (BDAnnotationPrior annKey1 line) indented
      BDAnnotationPrior annKey1 (BDSeq (l:lr)) ->
        Just $ BDSeq ((BDAnnotationPrior annKey1 l):lr)
      BDAnnotationPrior annKey1 (BDLines (l:lr)) ->
        Just $ BDLines ((BDAnnotationPrior annKey1 l):lr)
      BDAnnotationPrior annKey1 (BDCols sig (l:lr)) ->
        Just $ BDCols sig ((BDAnnotationPrior annKey1 l):lr)
      -- EnsureIndent float-in
      -- BDEnsureIndent indent (BDCols sig (col:colr)) ->
      --   Just $ BDCols sig (BDEnsureIndent indent col : (BDAddBaseY indent <$> colr))
      -- not sure if the following rule is necessary; tests currently are
      -- unaffected.
      -- BDEnsureIndent indent (BDLines lines) ->
      --   Just $ BDLines $ BDEnsureIndent indent <$> lines
      -- post floating in
      BDAnnotationRest annKey1 (BDPar ind line indented) ->
        Just $ BDPar ind line $ BDAnnotationRest annKey1 indented
      BDAnnotationRest annKey1 (BDSeq list) ->
        Just $ BDSeq $ List.init list ++ [BDAnnotationRest annKey1 $ List.last list]
      BDAnnotationRest annKey1 (BDLines list) ->
        Just $ BDLines $ List.init list ++ [BDAnnotationRest annKey1 $ List.last list]
      BDAnnotationRest annKey1 (BDCols sig cols) ->
        Just $ BDCols sig $ List.init cols ++ [BDAnnotationRest annKey1 $ List.last cols]
      _ -> Nothing
