{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Brittany.Internal.LayouterBasics
  ( processDefault
  , rdrNameToText
  , lrdrNameToText
  , lrdrNameToTextAnn
  , lrdrNameToTextAnnTypeEqualityIsSpecial
  , lrdrNameToTextAnnTypeEqualityIsSpecialAndRespectTick
  , askIndent
  , extractAllComments
  , extractRestComments
  , filterAnns
  , docEmpty
  , docLit
  , docLitS
  , docAlt
  , CollectAltM
  , addAlternativeCond
  , addAlternative
  , runFilteredAlternative
  , docLines
  , docCols
  , docSeq
  , docPar
  , docNodeAnnKW
  , docNodeMoveToKWDP
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
  , docMoveToKWDP
  , docNonBottomSpacing
  , docNonBottomSpacingS
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
  , docParenL
  , docParenR
  , docParenHashLSep
  , docParenHashRSep
  , docBracketL
  , docBracketR
  , docTick
  , spacifyDocs
  , briDocMToPPM
  , allocateNode
  , docSharedWrapper
  , hasAnyCommentsBelow
  , hasAnyCommentsConnected
  , hasAnyCommentsPrior
  , hasAnyRegularCommentsConnected
  , hasAnyRegularCommentsRest
  , hasAnnKeywordComment
  , hasAnnKeyword
  )
where



#include "prelude.inc"

import qualified Control.Monad.Writer.Strict as Writer

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
    _    -> mTell $ Text.Builder.fromString str

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
        mTell [ErrorUnknownNode infoStr ast]
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

lrdrNameToTextAnnGen
  :: (MonadMultiReader Config m, MonadMultiReader (Map AnnKey Annotation) m)
  => (Text -> Text)
  -> Located RdrName
  -> m Text
lrdrNameToTextAnnGen f ast@(L _ n) = do
  anns <- mAsk
  let t = f $ rdrNameToText n
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

lrdrNameToTextAnn
  :: (MonadMultiReader Config m, MonadMultiReader (Map AnnKey Annotation) m)
  => Located RdrName
  -> m Text
lrdrNameToTextAnn = lrdrNameToTextAnnGen id

lrdrNameToTextAnnTypeEqualityIsSpecial
  :: (MonadMultiReader Config m, MonadMultiReader (Map AnnKey Annotation) m)
  => Located RdrName
  -> m Text
lrdrNameToTextAnnTypeEqualityIsSpecial ast = do
  let f x = if x == Text.pack "Data.Type.Equality~"
        then Text.pack "~" -- rraaaahhh special casing rraaahhhhhh
        else x
  lrdrNameToTextAnnGen f ast

-- | Same as lrdrNameToTextAnnTypeEqualityIsSpecial, but also inspects
-- the annotations for a (parent) node for a tick to be added to the
-- literal.
-- Excessively long name to reflect on us having to work around such
-- excessively obscure special cases in the exactprint API.
lrdrNameToTextAnnTypeEqualityIsSpecialAndRespectTick
  :: ( Data ast
     , MonadMultiReader Config m
     , MonadMultiReader (Map AnnKey Annotation) m
     )
  => Located ast
  -> Located RdrName
  -> m Text
lrdrNameToTextAnnTypeEqualityIsSpecialAndRespectTick ast1 ast2 = do
  hasQuote <- hasAnnKeyword ast1 AnnSimpleQuote
  x        <- lrdrNameToTextAnn ast2
  let lit = if x == Text.pack "Data.Type.Equality~"
        then Text.pack "~" -- rraaaahhh special casing rraaahhhhhh
        else x
  return $ if hasQuote then Text.cons '\'' lit else lit

askIndent :: (MonadMultiReader Config m) => m Int
askIndent = confUnpack . _lconfig_indentAmount . _conf_layout <$> mAsk


extractAllComments
  :: ExactPrint.Annotation -> [(ExactPrint.Comment, ExactPrint.DeltaPos)]
extractAllComments ann =
  ExactPrint.annPriorComments ann ++ extractRestComments ann

extractRestComments
  :: ExactPrint.Annotation -> [(ExactPrint.Comment, ExactPrint.DeltaPos)]
extractRestComments ann =
  ExactPrint.annFollowingComments ann
    ++ (ExactPrint.annsDP ann >>= \case
         (ExactPrint.AnnComment com, dp) -> [(com, dp)]
         _                               -> []
       )

filterAnns :: Data.Data.Data ast => ast -> ExactPrint.Anns -> ExactPrint.Anns
filterAnns ast =
  Map.filterWithKey (\k _ -> k `Set.member` foldedAnnKeys ast)

-- | True if there are any comments that are
-- a) connected to any node below (in AST sense) the given node AND
-- b) after (in source code order) the node.
hasAnyCommentsBelow :: Data ast => GHC.Located ast -> ToBriDocM Bool
hasAnyCommentsBelow ast@(L l _) =
  List.any (\(c, _) -> ExactPrint.commentIdentifier c > l)
    <$> astConnectedComments ast

-- | True if there are any comments that are connected to any node below (in AST
--   sense) the given node
hasAnyCommentsConnected :: Data ast => GHC.Located ast -> ToBriDocM Bool
hasAnyCommentsConnected ast = not . null <$> astConnectedComments ast

-- | True if there are any regular comments connected to any node below (in AST
--   sense) the given node
hasAnyRegularCommentsConnected :: Data ast => GHC.Located ast -> ToBriDocM Bool
hasAnyRegularCommentsConnected ast =
  any isRegularComment <$> astConnectedComments ast

-- | Regular comments are comments that are actually "source code comments",
-- i.e. things that start with "--" or "{-". In contrast to comment-annotations
-- used by ghc-exactprint for capturing symbols (and their exact positioning).
--
-- Only the type instance layouter makes use of this filter currently, but
-- it might make sense to apply it more aggressively or make it the default -
-- I believe that most of the time we branch on the existence of comments, we
-- only care about "regular" comments. We simply did not need the distinction
-- because "irregular" comments are not that common outside of type/data decls.
isRegularComment :: (ExactPrint.Comment, ExactPrint.DeltaPos) -> Bool
isRegularComment = (== Nothing) . ExactPrint.Types.commentOrigin . fst

astConnectedComments
  :: Data ast
  => GHC.Located ast
  -> ToBriDocM [(ExactPrint.Types.Comment, ExactPrint.Types.DeltaPos)]
astConnectedComments ast = do
  anns <- filterAnns ast <$> mAsk
  pure $ extractAllComments =<< Map.elems anns

hasAnyCommentsPrior :: Data ast => GHC.Located ast -> ToBriDocM Bool
hasAnyCommentsPrior ast = astAnn ast <&> \case
  Nothing -> False
  Just (ExactPrint.Types.Ann _ priors _ _ _ _) -> not $ null priors

hasAnyRegularCommentsRest :: Data ast => GHC.Located ast -> ToBriDocM Bool
hasAnyRegularCommentsRest ast = astAnn ast <&> \case
  Nothing -> False
  Just ann -> any isRegularComment (extractRestComments ann)

hasAnnKeywordComment
  :: Data ast => GHC.Located ast -> AnnKeywordId -> ToBriDocM Bool
hasAnnKeywordComment ast annKeyword = astAnn ast <&> \case
  Nothing  -> False
  Just ann -> any hasK (extractAllComments ann)
  where hasK = (== Just annKeyword) . ExactPrint.Types.commentOrigin . fst

hasAnnKeyword
  :: (Data a, MonadMultiReader (Map AnnKey Annotation) m)
  => Located a
  -> AnnKeywordId
  -> m Bool
hasAnnKeyword ast annKeyword = astAnn ast <&> \case
  Nothing -> False
  Just (ExactPrint.Types.Ann _ _ _ aks _ _) -> any hasK aks
 where
  hasK (ExactPrint.Types.G x, _) = x == annKeyword
  hasK _                         = False

astAnn
  :: (Data ast, MonadMultiReader (Map AnnKey Annotation) m)
  => GHC.Located ast
  -> m (Maybe Annotation)
astAnn ast = Map.lookup (ExactPrint.Types.mkAnnKey ast) <$> mAsk

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

docLitS :: String -> ToBriDocM BriDocNumbered
docLitS s = allocateNode $ BDFLit $ Text.pack s

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

newtype CollectAltM a = CollectAltM (Writer.Writer [ToBriDocM BriDocNumbered] a)
  deriving (Functor, Applicative, Monad)

addAlternativeCond :: Bool -> ToBriDocM BriDocNumbered -> CollectAltM ()
addAlternativeCond cond doc =
  when cond (addAlternative doc)

addAlternative :: ToBriDocM BriDocNumbered -> CollectAltM ()
addAlternative =
  CollectAltM . Writer.tell . (: [])

runFilteredAlternative :: CollectAltM () -> ToBriDocM BriDocNumbered
runFilteredAlternative (CollectAltM action) =
  docAlt $ Writer.execWriter action


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

docMoveToKWDP
  :: AnnKey
  -> AnnKeywordId
  -> Bool
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docMoveToKWDP annKey kw shouldRestoreIndent bdm =
  allocateNode . BDFMoveToKWDP annKey kw shouldRestoreIndent =<< bdm

docAnnotationRest
  :: AnnKey -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAnnotationRest annKey bdm = allocateNode . BDFAnnotationRest annKey =<< bdm

docNonBottomSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docNonBottomSpacing bdm = allocateNode . BDFNonBottomSpacing False =<< bdm

docNonBottomSpacingS :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docNonBottomSpacingS bdm = allocateNode . BDFNonBottomSpacing True =<< bdm

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
docParenLSep = appSep docParenL

-- TODO: we don't make consistent use of these (yet). However, I think the
-- most readable approach overall might be something else: define
-- `lit = docLit . Text.pack` and `prepSep = docSeq [docSeparator, x]`.
-- I think those two would make the usage most readable.
-- lit "("  and  appSep (lit "(")  are understandable and short without
-- introducing a new top-level binding for all types of parentheses.
docParenL :: ToBriDocM BriDocNumbered
docParenL = docLit $ Text.pack "("

docParenR :: ToBriDocM BriDocNumbered
docParenR = docLit $ Text.pack ")"

docParenHashLSep :: ToBriDocM BriDocNumbered
docParenHashLSep =  docSeq [docLit $ Text.pack "(#", docSeparator]

docParenHashRSep :: ToBriDocM BriDocNumbered
docParenHashRSep = docSeq [docSeparator, docLit $ Text.pack "#)"]

docBracketL :: ToBriDocM BriDocNumbered
docBracketL = docLit $ Text.pack "["

docBracketR :: ToBriDocM BriDocNumbered
docBracketR = docLit $ Text.pack "]"


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

docNodeMoveToKWDP
  :: Data.Data.Data ast
  => Located ast
  -> AnnKeywordId
  -> Bool
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docNodeMoveToKWDP ast kw shouldRestoreIndent bdm =
  docMoveToKWDP (ExactPrint.Types.mkAnnKey ast) kw shouldRestoreIndent bdm

class DocWrapable a where
  docWrapNode :: ( Data.Data.Data ast)
              => Located ast
              -> a
              -> a
  docWrapNodePrior :: ( Data.Data.Data ast)
                   => Located ast
                   -> a
                   -> a
  docWrapNodeRest  :: ( Data.Data.Data ast)
                   => Located ast
                   -> a
                   -> a

instance DocWrapable (ToBriDocM BriDocNumbered) where
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

instance DocWrapable (ToBriDocM a) => DocWrapable [ToBriDocM a] where
  docWrapNode ast bdms = case bdms of
    [] -> []
    [bd] -> [docWrapNode ast bd]
    (bd1:bdR) | (bdN:bdM) <- reverse bdR ->
      [docWrapNodePrior ast bd1] ++ reverse bdM ++ [docWrapNodeRest ast bdN]
    _ -> error "cannot happen (TM)"
  docWrapNodePrior ast bdms = case bdms of
    [] -> []
    [bd] -> [docWrapNodePrior ast bd]
    (bd1:bdR) -> docWrapNodePrior ast bd1 : bdR
  docWrapNodeRest ast bdms = case reverse bdms of
      [] -> []
      (bdN:bdR) -> reverse $ docWrapNodeRest ast bdN : bdR

instance DocWrapable (ToBriDocM a) => DocWrapable (ToBriDocM [a]) where
  docWrapNode ast bdsm = do
    bds <- bdsm
    case bds of
      [] -> return [] -- TODO: this might be bad. maybe. then again, not really. well.
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
      [] -> return []
      (bd1:bdR) -> do
        bd1' <- docWrapNodePrior ast (return bd1)
        return (bd1':bdR)
  docWrapNodeRest ast bdsm = do
    bds <- bdsm
    case reverse bds of
      [] -> return []
      (bdN:bdR) -> do
        bdN' <- docWrapNodeRest ast (return bdN)
        return $ reverse (bdN':bdR)

instance DocWrapable (ToBriDocM a) => DocWrapable (ToBriDocM (Seq a)) where
  docWrapNode ast bdsm = do
    bds <- bdsm
    case Seq.viewl bds of
      Seq.EmptyL -> return Seq.empty -- TODO: this might be bad. maybe. then again, not really. well.
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
      Seq.EmptyL -> return Seq.empty
      bd1 Seq.:< bdR -> do
        bd1' <- docWrapNodePrior ast (return bd1)
        return $ bd1' Seq.<| bdR
  docWrapNodeRest ast bdsm = do
    bds <- bdsm
    case Seq.viewr bds of
      Seq.EmptyR -> return Seq.empty
      bdR Seq.:> bdN -> do
        bdN' <- docWrapNodeRest ast (return bdN)
        return $ bdR Seq.|> bdN'

instance DocWrapable (ToBriDocM ([BriDocNumbered], BriDocNumbered, a)) where
  docWrapNode ast stuffM = do
    (bds, bd, x) <- stuffM
    if null bds
      then do
        bd' <- docWrapNode ast (return bd)
        return (bds, bd', x)
      else do
        bds' <- docWrapNodePrior ast (return bds)
        bd' <- docWrapNodeRest ast (return bd)
        return (bds', bd', x)
  docWrapNodePrior ast stuffM = do
    (bds, bd, x) <- stuffM
    bds' <- docWrapNodePrior ast (return bds)
    return (bds', bd, x)
  docWrapNodeRest ast stuffM = do
    (bds, bd, x) <- stuffM
    bd' <- docWrapNodeRest ast (return bd)
    return (bds, bd', x)



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
  :: Data.Data.Data ast
  => String
  -> GenLocated GHC.SrcSpan ast
  -> ToBriDocM BriDocNumbered
unknownNodeError infoStr ast = do
  mTell [ErrorUnknownNode infoStr ast]
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
