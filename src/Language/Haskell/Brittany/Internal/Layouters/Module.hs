{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Brittany.Internal.Layouters.Module (layoutModule) where

#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
import           Language.Haskell.Brittany.Internal.Layouters.Import
import           Language.Haskell.Brittany.Internal.Config.Types

import GHC (unLoc, runGhc, GenLocated(L), moduleNameString, AnnKeywordId(..))
#if MIN_VERSION_ghc(8,10,1)   /* ghc-8.10.1 */
import           GHC.Hs
import           GHC.Hs.ImpExp
#else
import           HsSyn
import           HsImpExp
#endif
import           Name
import           FieldLabel
import qualified FastString
import           BasicTypes
import           Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import           Language.Haskell.GHC.ExactPrint.Types
                                                ( DeltaPos(..)
                                                , deltaRow
                                                , commentContents
                                                )

import           Language.Haskell.Brittany.Internal.Utils



layoutModule :: ToBriDoc HsModule
layoutModule lmod@(L _ mod') = case mod' of
    -- Implicit module Main
  HsModule Nothing  _   imports _ _ _ -> do
    commentedImports <- transformToCommentedImport imports
    -- groupify commentedImports `forM_` tellDebugMessShow
    docLines (commentedImportsToDoc <$> sortCommentedImports commentedImports)
    -- sortedImports <- sortImports imports
    -- docLines $ [layoutImport y i | (y, i) <- sortedImports]
  HsModule (Just n) les imports _ _ _ -> do
    commentedImports <- transformToCommentedImport imports
    -- groupify commentedImports `forM_` tellDebugMessShow
    -- sortedImports <- sortImports imports
    let tn = Text.pack $ moduleNameString $ unLoc n
    allowSingleLineExportList <- mAsk
      <&> _conf_layout
      .>  _lconfig_allowSingleLineExportList
      .>  confUnpack
    -- the config should not prevent single-line layout when there is no
    -- export list
    let allowSingleLine = allowSingleLineExportList || Data.Maybe.isNothing les
    docLines
      $ docSeq
          [ docNodeAnnKW lmod Nothing docEmpty
             -- A pseudo node that serves merely to force documentation
             -- before the node
          , docNodeMoveToKWDP lmod AnnModule True $ runFilteredAlternative $ do
            addAlternativeCond allowSingleLine $
              docForceSingleline
                $ docSeq
                [ appSep $ docLit $ Text.pack "module"
                , appSep $ docLit tn
                , docWrapNode lmod $ appSep $ case les of
                  Nothing -> docEmpty
                  Just x  -> layoutLLIEs True KeepItemsUnsorted x
                , docSeparator
                , docLit $ Text.pack "where"
                ]
            addAlternative
              $ docLines
              [ docAddBaseY BrIndentRegular $ docPar
                (docSeq [appSep $ docLit $ Text.pack "module", docLit tn]
                )
                (docSeq [
                          docWrapNode lmod $ case les of
                           Nothing -> docEmpty
                           Just x  -> layoutLLIEs False KeepItemsUnsorted x
                        , docSeparator
                        , docLit $ Text.pack "where"
                        ]
                )
              ]
          ]
      : (commentedImportsToDoc <$> sortCommentedImports commentedImports) -- [layoutImport y i | (y, i) <- sortedImports]

data CommentedImport
  = EmptyLine
  | IndependentComment (Comment, DeltaPos)
  | ImportStatement ImportStatementRecord

instance Show CommentedImport where
  show = \case
    EmptyLine            -> "EmptyLine"
    IndependentComment _ -> "IndependentComment"
    ImportStatement r ->
      "ImportStatement " ++ show (length $ commentsBefore r) ++ " " ++ show
        (length $ commentsAfter r)

data ImportStatementRecord = ImportStatementRecord
  { commentsBefore :: [(Comment, DeltaPos)]
  , commentsAfter :: [(Comment, DeltaPos)]
  , importStatement :: ImportDecl GhcPs
  }

instance Show ImportStatementRecord where
  show r = "ImportStatement " ++ show (length $ commentsBefore r) ++ " " ++ show
        (length $ commentsAfter r)

transformToCommentedImport
  :: [LImportDecl GhcPs] -> ToBriDocM [CommentedImport]
transformToCommentedImport is = do
  nodeWithAnnotations <- is `forM` \i@(L _ rawImport) -> do
    annotionMay <- astAnn i
    pure (annotionMay, rawImport)
  let
    convertComment (c, DP (y, x)) =
      replicate (y - 1) EmptyLine ++ [IndependentComment (c, DP (1, x))]
    accumF
      :: [(Comment, DeltaPos)]
      -> (Maybe Annotation, ImportDecl GhcPs)
      -> ([(Comment, DeltaPos)], [CommentedImport])
    accumF accConnectedComm (annMay, decl) = case annMay of
      Nothing ->
        ( []
        , [ ImportStatement ImportStatementRecord { commentsBefore  = []
                                                  , commentsAfter   = []
                                                  , importStatement = decl
                                                  }
          ]
        )
      Just ann ->
        let
          blanksBeforeImportDecl = deltaRow (annEntryDelta ann) - 1
          (newAccumulator, priorComments') =
            List.span ((== 0) . deltaRow . snd) (annPriorComments ann)
          go
            :: [(Comment, DeltaPos)]
            -> [(Comment, DeltaPos)]
            -> ([CommentedImport], [(Comment, DeltaPos)], Int)
          go acc []                       = ([], acc, 0)
          go acc [c1@(_, DP (y, _))] = ([], c1 : acc, y - 1)
          go acc (c1@(_, DP (1, _)) : xs) = go (c1 : acc) xs
          go acc ((c1, DP (y, x)) : xs) =
            ( (convertComment =<< xs) ++ replicate (y - 1) EmptyLine
            , (c1, DP (1, x)) : acc
            , 0
            )
          (convertedIndependentComments, beforeComments, initialBlanks) =
            if blanksBeforeImportDecl /= 0
              then (convertComment =<< priorComments', [], 0)
              else go [] (reverse priorComments')
        in
          ( newAccumulator
          , convertedIndependentComments
          ++ replicate (blanksBeforeImportDecl + initialBlanks) EmptyLine
          ++ [ ImportStatement ImportStatementRecord
                 { commentsBefore  = beforeComments
                 , commentsAfter   = accConnectedComm
                 , importStatement = decl
                 }
             ]
          )
  let (finalAcc, finalList) = mapAccumR accumF [] nodeWithAnnotations
  pure $ join $ (convertComment =<< finalAcc) : finalList

sortCommentedImports :: [CommentedImport] -> [CommentedImport]
sortCommentedImports =
  unpackImports . mergeGroups . map (fmap (sortGroups)) . groupify
 where
  unpackImports :: [CommentedImport] -> [CommentedImport]
  unpackImports xs = xs >>= \case
    l@EmptyLine            -> [l]
    l@IndependentComment{} -> [l]
    ImportStatement r ->
      map IndependentComment (commentsBefore r) ++ [ImportStatement r]
  mergeGroups
    :: [Either CommentedImport [ImportStatementRecord]] -> [CommentedImport]
  mergeGroups xs = xs >>= \case
    Left  x -> [x]
    Right y -> ImportStatement <$> y
  sortGroups :: [ImportStatementRecord] -> [ImportStatementRecord]
  sortGroups =
    List.sortOn (moduleNameString . unLoc . ideclName . importStatement)
  groupify
    :: [CommentedImport] -> [Either CommentedImport [ImportStatementRecord]]
  groupify cs = go [] cs
   where
    go [] = \case
      (l@EmptyLine            : rest) -> Left l : go [] rest
      (l@IndependentComment{} : rest) -> Left l : go [] rest
      (ImportStatement r      : rest) -> go [r] rest
      []                              -> []
    go acc = \case
      (l@EmptyLine : rest) -> Right (reverse acc) : Left l : go [] rest
      (l@IndependentComment{} : rest) ->
        Left l : Right (reverse acc) : go [] rest
      (ImportStatement r : rest) -> go (r : acc) rest
      []                         -> [Right (reverse acc)]

commentedImportsToDoc :: CommentedImport -> ToBriDocM BriDocNumbered
commentedImportsToDoc = \case
  EmptyLine -> docLitS ""
  IndependentComment c -> commentToDoc c
  ImportStatement r ->
    docSeq
      ( layoutImport (importStatement r)
      : map commentToDoc (commentsAfter r)
      )
 where
  commentToDoc (c, DP (_y, x)) = docLitS (replicate x ' ' ++ commentContents c)
