module Language.Haskell.Brittany.Internal.LayouterBasics
  ( processDefault
  , rdrNameToText
  , lrdrNameToText
  , lrdrNameToTextAnn
  , lrdrNameToTextAnnTypeEqualityIsSpecial
  , askIndent
  , extractAllComments
  , filterAnns
  , docEmpty
  , docLit
  , docAlt
  , docAltFilter
  , docLines
  , docCols
  , docSeq
  , docPar
  , docNodeAnnKW
  , docWrapNode
  , docWrapNodePrior
  , docWrapNodeRest
  , docForceSingleline
  , docForceMultiline
  , docEnsureIndent
  , docAddBaseY
  , docSetBaseY
  , docSetIndentLevel
  , docSeparator
  , docAnnotationPrior
  , docAnnotationKW
  , docAnnotationRest
  , docNonBottomSpacing
  , docSetParSpacing
  , docForceParSpacing
  , docDebug
  , docSetBaseAndIndent
  , briDocByExact
  , briDocByExactNoComment
  , briDocByExactInlineOnly
  , foldedAnnKeys
  , unknownNodeError
  , appSep
  , docCommaSep
  , docParenLSep
  , docParenR
  , docTick
  , spacifyDocs
  , briDocMToPPM
  , allocateNode
  , docSharedWrapper
  , hasAnyCommentsBelow
  , hasAnnKeyword
  )
where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint.Utils

import Language.Haskell.GHC.ExactPrint.Types ( AnnKey, Annotation, KeywordId )

import qualified Data.Text.Lazy.Builder as Text.Builder

import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import Language.Haskell.Brittany.Internal.ExactPrintUtils

import           RdrName ( RdrName(..) )
import           GHC ( Located, runGhc, GenLocated(L), moduleNameString )
import qualified SrcLoc        as GHC
import           OccName ( occNameString )
import           Name ( getOccString )
import           Module ( moduleName )
import           ApiAnnotation ( AnnKeywordId(..) )

import           Data.Data
import           Data.Generics.Schemes

import qualified Data.Char as Char

import           DataTreePrint

import           Data.HList.HList



processDefault
  :: ( ExactPrint.Annotate.Annotate ast
     , MonadMultiWriter Text.Builder.Builder m
     , MonadMultiReader ExactPrint.Types.Anns m
     )
  => Located ast
  -> m ()
processDefault x = do
  anns <- mAsk
  let str = ExactPrint.exactPrint x anns
  -- this hack is here so our print-empty-module trick does not add
  -- a newline at the start if there actually is no module header / imports
  -- / anything.
  -- TODO: instead the appropriate annotation could be removed when "cleaning"
  --       the module (header). This would remove the need for this hack!
  case str of
    "\n" -> return ()
    _    -> mTell $ Text.Builder.fromString $ str

-- | Use ExactPrint's output for this node; add a newly generated inline comment
-- at insertion position (meant to point out to the user that this node is
-- not handled by brittany yet). Useful when starting implementing new
-- syntactic constructs when children are not handled yet.
briDocByExact
  :: (ExactPrint.Annotate.Annotate ast)
  => Located ast
  -> ToBriDocM BriDocNumbered
briDocByExact ast = do
  anns <- mAsk
  traceIfDumpConf "ast"
                  _dconf_dump_ast_unknown
                  (printTreeWithCustom 100 (customLayouterF anns) ast)
  docExt ast anns True

-- | Use ExactPrint's output for this node.
-- Consider that for multi-line input, the indentation of the code produced
-- by ExactPrint might be different, and even incompatible with the indentation
-- of its surroundings as layouted by brittany. But there are safe uses of
-- this, e.g. for any top-level declarations.
briDocByExactNoComment
  :: (ExactPrint.Annotate.Annotate ast)
  => Located ast
  -> ToBriDocM BriDocNumbered
briDocByExactNoComment ast = do
  anns <- mAsk
  traceIfDumpConf "ast"
                  _dconf_dump_ast_unknown
                  (printTreeWithCustom 100 (customLayouterF anns) ast)
  docExt ast anns False

-- | Use ExactPrint's output for this node, presuming that this output does
-- not contain any newlines. If this property is not met, the semantics
-- depend on the @econf_AllowRiskyExactPrintUse@ config flag.
briDocByExactInlineOnly
  :: (ExactPrint.Annotate.Annotate ast, Data ast)
  => String
  -> Located ast
  -> ToBriDocM BriDocNumbered
briDocByExactInlineOnly infoStr ast = do
  anns <- mAsk
  traceIfDumpConf "ast"
                  _dconf_dump_ast_unknown
                  (printTreeWithCustom 100 (customLayouterF anns) ast)
  let exactPrinted = Text.pack $ ExactPrint.exactPrint ast anns
  fallbackMode <-
    mAsk <&> _conf_errorHandling .> _econf_ExactPrintFallback .> confUnpack
  let exactPrintNode t = allocateNode $ BDFExternal
        (ExactPrint.Types.mkAnnKey ast)
        (foldedAnnKeys ast)
        False
        t
  let errorAction = do
        mTell $ [ErrorUnknownNode infoStr ast]
        docLit
          $ Text.pack "{- BRITTANY ERROR UNHANDLED SYNTACTICAL CONSTRUCT -}"
  case (fallbackMode, Text.lines exactPrinted) of
    (ExactPrintFallbackModeNever, _  ) -> errorAction
    (_                          , [t]) -> exactPrintNode
      (Text.dropWhile Char.isSpace . Text.dropWhileEnd Char.isSpace $ t)
    (ExactPrintFallbackModeRisky, _) -> exactPrintNode exactPrinted
    _ -> errorAction

rdrNameToText :: RdrName -> Text
-- rdrNameToText = Text.pack . show . flip runSDoc unsafeGlobalDynFlags . ppr
rdrNameToText (Unqual occname) = Text.pack $ occNameString occname
rdrNameToText (Qual mname occname) =
  Text.pack $ moduleNameString mname ++ "." ++ occNameString occname
rdrNameToText (Orig modul occname) =
  Text.pack $ moduleNameString (moduleName modul) ++ occNameString occname
rdrNameToText (Exact name) = Text.pack $ getOccString name

lrdrNameToText :: GenLocated l RdrName -> Text
lrdrNameToText (L _ n) = rdrNameToText n

lrdrNameToTextAnn
  :: (MonadMultiReader Config m, MonadMultiReader (Map AnnKey Annotation) m)
  => Located RdrName
  -> m Text
lrdrNameToTextAnn ast@(L _ n) = do
  anns <- mAsk
  let t = rdrNameToText n
  let hasUni x (ExactPrint.Types.G y, _) = x == y
      hasUni _ _                         = False
  -- TODO: in general: we should _always_ process all annotaiton stuff here.
  --       whatever we don't probably should have had some effect on the
  --       output. in such cases, resorting to byExact is probably the safe
  --       choice.
  return $ case Map.lookup (ExactPrint.Types.mkAnnKey ast) anns of
    Nothing                                   -> t
    Just (ExactPrint.Types.Ann _ _ _ aks _ _) -> case n of
      Exact{} | t == Text.pack "()"      -> t
      _ | any (hasUni AnnBackquote) aks  -> Text.pack "`" <> t <> Text.pack "`"
      _ | any (hasUni AnnCommaTuple) aks -> t
      _ | any (hasUni AnnOpenP) aks      -> Text.pack "(" <> t <> Text.pack ")"
      _ | otherwise                      -> t

lrdrNameToTextAnnTypeEqualityIsSpecial
  :: (MonadMultiReader Config m, MonadMultiReader (Map AnnKey Annotation) m)
  => Located RdrName
  -> m Text
lrdrNameToTextAnnTypeEqualityIsSpecial ast = do
  x <- lrdrNameToTextAnn ast
  return $ if x == Text.pack "Data.Type.Equality~"
    then Text.pack "~" -- rraaaahhh special casing rraaahhhhhh
    else x

askIndent :: (MonadMultiReader Config m) => m Int
askIndent = confUnpack . _lconfig_indentAmount . _conf_layout <$> mAsk


extractAllComments
  :: ExactPrint.Annotation -> [(ExactPrint.Comment, ExactPrint.DeltaPos)]
extractAllComments ann =
  ExactPrint.annPriorComments ann
    ++ ExactPrint.annFollowingComments ann
    ++ ( ExactPrint.annsDP ann >>= \case
         (ExactPrint.AnnComment com, dp) -> [(com, dp)]
         _                               -> []
       )

filterAnns :: Data.Data.Data ast => ast -> ExactPrint.Anns -> ExactPrint.Anns
filterAnns ast anns =
  Map.filterWithKey (\k _ -> k `Set.member` foldedAnnKeys ast) anns

hasAnyCommentsBelow :: Data ast => GHC.Located ast -> ToBriDocM Bool
hasAnyCommentsBelow ast@(L l _) = do
  anns <- filterAnns ast <$> mAsk
  return
    $ List.any (\(c, _) -> ExactPrint.commentIdentifier c > l)
    $ (=<<) extractAllComments
    $ Map.elems
    $ anns

hasAnnKeyword
  :: (Data a, MonadMultiReader (Map AnnKey Annotation) m)
  => Located a
  -> AnnKeywordId
  -> m Bool
hasAnnKeyword ast annKeyword = do
  anns <- mAsk
  let hasK (ExactPrint.Types.G x, _) = x == annKeyword
      hasK _                         = False
  pure $ case Map.lookup (ExactPrint.Types.mkAnnKey ast) anns of
    Nothing -> False
    Just (ExactPrint.Types.Ann _ _ _ aks _ _) -> any hasK aks

-- new BriDoc stuff

allocateNode
  :: MonadMultiState NodeAllocIndex m => BriDocFInt -> m BriDocNumbered
allocateNode bd = do
  i <- allocNodeIndex
  return (i, bd)

allocNodeIndex :: MonadMultiState NodeAllocIndex m => m Int
allocNodeIndex = do
  NodeAllocIndex i <- mGet
  mSet $ NodeAllocIndex (i + 1)
  return i

-- docEmpty :: MonadMultiState NodeAllocIndex m => m BriDocNumbered
-- docEmpty = allocateNode BDFEmpty
-- 
-- docLit :: MonadMultiState NodeAllocIndex m => Text -> m BriDocNumbered
-- docLit t = allocateNode $ BDFLit t
-- 
-- docExt :: (ExactPrint.Annotate.Annotate ast, MonadMultiState NodeAllocIndex m)
--        => Located ast -> ExactPrint.Types.Anns -> Bool -> m BriDocNumbered
-- docExt x anns shouldAddComment = allocateNode $ BDFExternal
--                   (ExactPrint.Types.mkAnnKey x)
--                   (foldedAnnKeys x)
--                   shouldAddComment
--                   (Text.pack $ ExactPrint.exactPrint x anns)
-- 
-- docAlt :: MonadMultiState NodeAllocIndex m => [m BriDocNumbered] -> m BriDocNumbered
-- docAlt l = allocateNode . BDFAlt =<< sequence l
-- 
-- 
-- docSeq :: MonadMultiState NodeAllocIndex m => [m BriDocNumbered] -> m BriDocNumbered
-- docSeq l = allocateNode . BDFSeq =<< sequence l
-- 
-- docLines :: MonadMultiState NodeAllocIndex m => [m BriDocNumbered] -> m BriDocNumbered
-- docLines l = allocateNode . BDFLines =<< sequence l
-- 
-- docCols :: MonadMultiState NodeAllocIndex m => ColSig -> [m BriDocNumbered] -> m BriDocNumbered
-- docCols sig l = allocateNode . BDFCols sig =<< sequence l
-- 
-- docAddBaseY :: MonadMultiState NodeAllocIndex m => BrIndent -> m BriDocNumbered -> m BriDocNumbered
-- docAddBaseY ind bdm = allocateNode . BDFAddBaseY ind =<< bdm
-- 
-- docSetBaseY :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docSetBaseY bdm = allocateNode . BDFSetBaseY =<< bdm
-- 
-- docSetIndentLevel :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docSetIndentLevel bdm = allocateNode . BDFSetIndentLevel =<< bdm
-- 
-- docSeparator :: MonadMultiState NodeAllocIndex m => m BriDocNumbered
-- docSeparator = allocateNode BDFSeparator
-- 
-- docAnnotationPrior :: MonadMultiState NodeAllocIndex m => AnnKey -> m BriDocNumbered -> m BriDocNumbered
-- docAnnotationPrior annKey bdm = allocateNode . BDFAnnotationPrior annKey =<< bdm
-- 
-- docAnnotationPost :: MonadMultiState NodeAllocIndex m => AnnKey -> m BriDocNumbered -> m BriDocNumbered
-- docAnnotationPost  annKey bdm = allocateNode . BDFAnnotationPost annKey =<< bdm
-- 
-- docNonBottomSpacing :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docNonBottomSpacing bdm = allocateNode . BDFNonBottomSpacing =<< bdm
-- 
-- appSep :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- appSep x = docSeq [x, docSeparator]
-- 
-- docCommaSep :: MonadMultiState NodeAllocIndex m => m BriDocNumbered
-- docCommaSep = appSep $ docLit $ Text.pack ","
-- 
-- docParenLSep :: MonadMultiState NodeAllocIndex m => m BriDocNumbered
-- docParenLSep = appSep $ docLit $ Text.pack "("
-- 
-- 
-- docPostComment :: (Data.Data.Data ast, MonadMultiState NodeAllocIndex m)
--                => Located ast
--                -> m BriDocNumbered
--                -> m BriDocNumbered
-- docPostComment ast bdm = do
--   bd <- bdm
--   allocateNode $ BDFAnnotationPost (ExactPrint.Types.mkAnnKey ast) bd
-- 
-- docWrapNode :: ( Data.Data.Data ast, MonadMultiState NodeAllocIndex m)
--             => Located ast
--             -> m BriDocNumbered
--             -> m BriDocNumbered
-- docWrapNode ast bdm = do
--   bd <- bdm
--   i1 <- allocNodeIndex
--   i2 <- allocNodeIndex
--   return
--     $ (,) i1
--     $ BDFAnnotationPrior (ExactPrint.Types.mkAnnKey ast)
--     $ (,) i2
--     $ BDFAnnotationPost (ExactPrint.Types.mkAnnKey ast)
--     $ bd
-- 
-- docPar :: MonadMultiState NodeAllocIndex m
--        => m BriDocNumbered
--        -> m BriDocNumbered
--        -> m BriDocNumbered
-- docPar lineM indentedM = do
--   line <- lineM
--   indented <- indentedM
--   allocateNode $ BDFPar BrIndentNone line indented
-- 
-- docForceSingleline :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docForceSingleline bdm = allocateNode . BDFForceSingleline =<< bdm
-- 
-- docForceMultiline :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docForceMultiline bdm = allocateNode . BDFForceMultiline =<< bdm
-- 
-- docEnsureIndent :: MonadMultiState NodeAllocIndex m => BrIndent -> m BriDocNumbered -> m BriDocNumbered
-- docEnsureIndent ind mbd = mbd >>= \bd -> allocateNode $ BDFEnsureIndent ind bd

docEmpty :: ToBriDocM BriDocNumbered
docEmpty = allocateNode BDFEmpty

docLit :: Text -> ToBriDocM BriDocNumbered
docLit t = allocateNode $ BDFLit t

docExt
  :: (ExactPrint.Annotate.Annotate ast)
  => Located ast
  -> ExactPrint.Types.Anns
  -> Bool
  -> ToBriDocM BriDocNumbered
docExt x anns shouldAddComment = allocateNode $ BDFExternal
  (ExactPrint.Types.mkAnnKey x)
  (foldedAnnKeys x)
  shouldAddComment
  (Text.pack $ ExactPrint.exactPrint x anns)

docAlt :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docAlt l = allocateNode . BDFAlt =<< sequence l

docAltFilter :: [(Bool, ToBriDocM BriDocNumbered)] -> ToBriDocM BriDocNumbered
docAltFilter = docAlt . map snd . filter fst


docSeq :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docSeq [] = docEmpty
docSeq l = allocateNode . BDFSeq =<< sequence l

docLines :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docLines l = allocateNode . BDFLines =<< sequence l

docCols :: ColSig -> [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docCols sig l = allocateNode . BDFCols sig =<< sequence l

docAddBaseY :: BrIndent -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAddBaseY ind bdm = allocateNode . BDFAddBaseY ind =<< bdm

docSetBaseY :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetBaseY bdm = do
  bd <- bdm
  -- the order here is important so that these two nodes can be treated
  -- properly over at `transformAlts`.
  n1 <- allocateNode $ BDFBaseYPushCur bd
  n2 <- allocateNode $ BDFBaseYPop n1
  return n2

docSetIndentLevel :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetIndentLevel bdm = do
  bd <- bdm
  n1 <- allocateNode $ BDFIndentLevelPushCur bd
  n2 <- allocateNode $ BDFIndentLevelPop n1
  return n2

docSetBaseAndIndent :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetBaseAndIndent = docSetBaseY . docSetIndentLevel

docSeparator :: ToBriDocM BriDocNumbered
docSeparator = allocateNode BDFSeparator

docAnnotationPrior
  :: AnnKey -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAnnotationPrior annKey bdm = allocateNode . BDFAnnotationPrior annKey =<< bdm

docAnnotationKW
  :: AnnKey
  -> Maybe AnnKeywordId
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docAnnotationKW annKey kw bdm = allocateNode . BDFAnnotationKW annKey kw =<< bdm

docAnnotationRest
  :: AnnKey -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAnnotationRest annKey bdm = allocateNode . BDFAnnotationRest annKey =<< bdm

docNonBottomSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docNonBottomSpacing bdm = allocateNode . BDFNonBottomSpacing =<< bdm

docSetParSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetParSpacing bdm = allocateNode . BDFSetParSpacing =<< bdm

docForceParSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceParSpacing bdm = allocateNode . BDFForceParSpacing =<< bdm

docDebug :: String -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docDebug s bdm = allocateNode . BDFDebug s =<< bdm

appSep :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
appSep x = docSeq [x, docSeparator]

docCommaSep :: ToBriDocM BriDocNumbered
docCommaSep = appSep $ docLit $ Text.pack ","

docParenLSep :: ToBriDocM BriDocNumbered
docParenLSep = appSep $ docLit $ Text.pack "("

docParenR :: ToBriDocM BriDocNumbered
docParenR = docLit $ Text.pack ")"

docTick :: ToBriDocM BriDocNumbered
docTick = docLit $ Text.pack "'"

docNodeAnnKW
  :: Data.Data.Data ast
  => Located ast
  -> Maybe AnnKeywordId
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docNodeAnnKW ast kw bdm =
  docAnnotationKW (ExactPrint.Types.mkAnnKey ast) kw bdm

class DocWrapable a where
  docWrapNode :: ( Data.Data.Data ast)
              => Located ast
              -> ToBriDocM a
              -> ToBriDocM a
  docWrapNodePrior :: ( Data.Data.Data ast)
                   => Located ast
                   -> ToBriDocM a
                   -> ToBriDocM a
  docWrapNodeRest  :: ( Data.Data.Data ast)
                   => Located ast
                   -> ToBriDocM a
                   -> ToBriDocM a

instance DocWrapable BriDocNumbered where
  docWrapNode ast bdm = do
    bd <- bdm
    i1 <- allocNodeIndex
    i2 <- allocNodeIndex
    return
      $ (,) i1
      $ BDFAnnotationPrior (ExactPrint.Types.mkAnnKey ast)
      $ (,) i2
      $ BDFAnnotationRest (ExactPrint.Types.mkAnnKey ast)
      $ bd
  docWrapNodePrior ast bdm = do
    bd <- bdm
    i1 <- allocNodeIndex
    return
      $ (,) i1
      $ BDFAnnotationPrior (ExactPrint.Types.mkAnnKey ast)
      $ bd
  docWrapNodeRest ast bdm = do
    bd <- bdm
    i2 <- allocNodeIndex
    return
      $ (,) i2
      $ BDFAnnotationRest (ExactPrint.Types.mkAnnKey ast)
      $ bd

instance DocWrapable a => DocWrapable [a] where
  docWrapNode ast bdsm = do
    bds <- bdsm
    case bds of
      [] -> return $ [] -- TODO: this might be bad. maybe. then again, not really. well.
      [bd] -> do
        bd' <- docWrapNode ast (return bd)
        return [bd']
      (bd1:bdR) | (bdN:bdM) <- reverse bdR -> do
        bd1' <- docWrapNodePrior ast (return bd1)
        bdN' <- docWrapNodeRest  ast (return bdN)
        return $ [bd1'] ++ reverse bdM ++ [bdN']
      _ -> error "cannot happen (TM)"
  docWrapNodePrior ast bdsm = do
    bds <- bdsm
    case bds of
      [] -> return $ []
      (bd1:bdR) -> do
        bd1' <- docWrapNodePrior ast (return bd1)
        return $ (bd1':bdR)
  docWrapNodeRest ast bdsm = do
    bds <- bdsm
    case reverse bds of
      [] -> return $ []
      (bdN:bdR) -> do
        bdN' <- docWrapNodeRest ast (return bdN)
        return $ reverse $ (bdN':bdR)

instance DocWrapable a => DocWrapable (Seq a) where
  docWrapNode ast bdsm = do
    bds <- bdsm
    case Seq.viewl bds of
      Seq.EmptyL -> return $ Seq.empty -- TODO: this might be bad. maybe. then again, not really. well.
      bd1 Seq.:< rest -> case Seq.viewr rest of
        Seq.EmptyR -> do
          bd1' <- docWrapNode ast (return bd1)
          return $ Seq.singleton bd1'
        bdM Seq.:> bdN -> do
          bd1' <- docWrapNodePrior ast (return bd1)
          bdN' <- docWrapNodeRest  ast (return bdN)
          return $ (bd1' Seq.<| bdM) Seq.|> bdN'
  docWrapNodePrior ast bdsm = do
    bds <- bdsm
    case Seq.viewl bds of
      Seq.EmptyL -> return $ Seq.empty
      bd1 Seq.:< bdR -> do
        bd1' <- docWrapNodePrior ast (return bd1)
        return $ bd1' Seq.<| bdR
  docWrapNodeRest ast bdsm = do
    bds <- bdsm
    case Seq.viewr bds of
      Seq.EmptyR -> return $ Seq.empty
      bdR Seq.:> bdN -> do
        bdN' <- docWrapNodeRest ast (return bdN)
        return $ bdR Seq.|> bdN'

instance DocWrapable ([BriDocNumbered], BriDocNumbered, a) where
  docWrapNode ast stuffM = do
    (bds, bd, x) <- stuffM
    if null bds
      then do
        bd' <- docWrapNode ast (return bd)
        return $ (bds, bd', x)
      else do
        bds' <- docWrapNodePrior ast (return bds)
        bd' <- docWrapNodeRest ast (return bd)
        return $ (bds', bd', x)
  docWrapNodePrior ast stuffM = do
    (bds, bd, x) <- stuffM
    bds' <- docWrapNodePrior ast (return bds)
    return $ (bds', bd, x)
  docWrapNodeRest ast stuffM = do
    (bds, bd, x) <- stuffM
    bd' <- docWrapNodeRest ast (return bd)
    return $ (bds, bd', x)



docPar
  :: ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docPar lineM indentedM = do
  line     <- lineM
  indented <- indentedM
  allocateNode $ BDFPar BrIndentNone line indented

docForceSingleline :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceSingleline bdm = allocateNode . BDFForceSingleline =<< bdm

docForceMultiline :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceMultiline bdm = allocateNode . BDFForceMultiline =<< bdm

docEnsureIndent
  :: BrIndent -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docEnsureIndent ind mbd = mbd >>= \bd -> allocateNode $ BDFEnsureIndent ind bd

unknownNodeError
  :: Data.Data.Data ast => String -> ast -> ToBriDocM BriDocNumbered
unknownNodeError infoStr ast = do
  mTell $ [ErrorUnknownNode infoStr ast]
  docLit $ Text.pack "{- BRITTANY ERROR UNHANDLED SYNTACTICAL CONSTRUCT -}"

spacifyDocs :: [ToBriDocM BriDocNumbered] -> [ToBriDocM BriDocNumbered]
spacifyDocs [] = []
spacifyDocs ds = fmap appSep (List.init ds) ++ [List.last ds]

briDocMToPPM :: ToBriDocM a -> PPMLocal a
briDocMToPPM m = do
  readers <- MultiRWSS.mGetRawR
  let ((x, errs), debugs) =
        runIdentity
          $ MultiRWSS.runMultiRWSTNil
          $ MultiRWSS.withMultiStateA (NodeAllocIndex 1)
          $ MultiRWSS.withMultiReaders readers
          $ MultiRWSS.withMultiWriterAW
          $ MultiRWSS.withMultiWriterAW
          $ m
  mTell debugs
  mTell errs
  return x

docSharedWrapper :: Monad m => (x -> m y) -> x -> m (m y)
docSharedWrapper f x = return <$> f x
