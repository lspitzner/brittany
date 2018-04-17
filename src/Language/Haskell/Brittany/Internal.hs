{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal
  ( parsePrintModule
  , parsePrintModuleTests
  , pPrintModule
  , pPrintModuleAndCheck
   -- re-export from utils:
  , parseModule
  , parseModuleFromString
  )
where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint.Parsers

import           Data.Data
import           Control.Monad.Trans.Except
import           Data.HList.HList
import           Data.CZipWith

import qualified Data.Text.Lazy.Builder as Text.Builder

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Config
import           Language.Haskell.Brittany.Internal.LayouterBasics

import           Language.Haskell.Brittany.Internal.Layouters.Type
import           Language.Haskell.Brittany.Internal.Layouters.Decl
import           Language.Haskell.Brittany.Internal.Layouters.Module
import           Language.Haskell.Brittany.Internal.Utils
import           Language.Haskell.Brittany.Internal.Backend
import           Language.Haskell.Brittany.Internal.BackendUtils
import           Language.Haskell.Brittany.Internal.ExactPrintUtils

import           Language.Haskell.Brittany.Internal.Transformations.Alt
import           Language.Haskell.Brittany.Internal.Transformations.Floating
import           Language.Haskell.Brittany.Internal.Transformations.Par
import           Language.Haskell.Brittany.Internal.Transformations.Columns
import           Language.Haskell.Brittany.Internal.Transformations.Indent

import qualified GHC as GHC hiding (parseModule)
import           ApiAnnotation ( AnnKeywordId(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           SrcLoc ( SrcSpan )
import           HsSyn
import qualified DynFlags as GHC
import qualified GHC.LanguageExtensions.Type as GHC



-- | Exposes the transformation in an pseudo-pure fashion. The signature
-- contains `IO` due to the GHC API not exposing a pure parsing function, but
-- there should be no observable effects.
--
-- Note that this function ignores/resets all config values regarding
-- debugging, i.e. it will never use `trace`/write to stderr.
--
-- Note that the ghc parsing function used internally currently is wrapped in
-- `mask_`, so cannot be killed easily. If you don't control the input, you
-- may wish to put some proper upper bound on the input's size as a timeout
-- won't do.
parsePrintModule :: Config -> Text -> IO (Either [BrittanyError] Text)
parsePrintModule configRaw inputText = runExceptT $ do
  let config = configRaw { _conf_debug = _conf_debug staticDefaultConfig }
  let ghcOptions         = config & _conf_forward & _options_ghc & runIdentity
  let config_pp          = config & _conf_preprocessor
  let cppMode            = config_pp & _ppconf_CPPMode & confUnpack
  let hackAroundIncludes = config_pp & _ppconf_hackAroundIncludes & confUnpack
  (anns, parsedSource, hasCPP) <- do
    let hackF s = if "#include" `isPrefixOf` s
          then "-- BRITTANY_INCLUDE_HACK " ++ s
          else s
    let hackTransform = if hackAroundIncludes
          then List.intercalate "\n" . fmap hackF . lines'
          else id
    let cppCheckFunc dynFlags = if GHC.xopt GHC.Cpp dynFlags
          then case cppMode of
            CPPModeAbort  -> return $ Left "Encountered -XCPP. Aborting."
            CPPModeWarn   -> return $ Right True
            CPPModeNowarn -> return $ Right True
          else return $ Right False
    parseResult <- lift $ parseModuleFromString
      ghcOptions
      "stdin"
      cppCheckFunc
      (hackTransform $ Text.unpack inputText)
    case parseResult of
      Left  err -> throwE [ErrorInput err]
      Right x   -> pure x
  (errsWarns, outputTextL) <- do
    let omitCheck =
          config
            & _conf_errorHandling
            & _econf_omit_output_valid_check
            & confUnpack
    (ews, outRaw) <- if hasCPP || omitCheck
      then return $ pPrintModule config anns parsedSource
      else lift $ pPrintModuleAndCheck config anns parsedSource
    let hackF s = fromMaybe s
          $ TextL.stripPrefix (TextL.pack "-- BRITTANY_INCLUDE_HACK ") s
    pure $ if hackAroundIncludes
      then
        ( ews
        , TextL.intercalate (TextL.pack "\n") $ fmap hackF $ TextL.splitOn
          (TextL.pack "\n")
          outRaw
        )
      else (ews, outRaw)
  let customErrOrder ErrorInput{}         = 4
      customErrOrder LayoutWarning{}      = 0 :: Int
      customErrOrder ErrorOutputCheck{}   = 1
      customErrOrder ErrorUnusedComment{} = 2
      customErrOrder ErrorUnknownNode{}   = 3
  let hasErrors =
        case config & _conf_errorHandling & _econf_Werror & confUnpack of
          False -> 0 < maximum (-1 : fmap customErrOrder errsWarns)
          True  -> not $ null errsWarns
  if hasErrors then throwE $ errsWarns else pure $ TextL.toStrict outputTextL



-- BrittanyErrors can be non-fatal warnings, thus both are returned instead
-- of an Either.
-- This should be cleaned up once it is clear what kinds of errors really
-- can occur.
pPrintModule
  :: Config
  -> ExactPrint.Anns
  -> GHC.ParsedSource
  -> ([BrittanyError], TextL.Text)
pPrintModule conf anns parsedModule =
  let
    ((out, errs), debugStrings) =
      runIdentity
        $ MultiRWSS.runMultiRWSTNil
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterW
        $ MultiRWSS.withMultiReader anns
        $ MultiRWSS.withMultiReader conf
        $ MultiRWSS.withMultiReader (extractToplevelAnns parsedModule anns)
        $ do
            traceIfDumpConf "bridoc annotations raw" _dconf_dump_annotations
              $ annsDoc anns
            ppModule parsedModule
    tracer =
      if Seq.null debugStrings
      then
        id
      else
        trace ("---- DEBUGMESSAGES ---- ")
          . foldr (seq . join trace) id debugStrings
  in
    tracer $ (errs, Text.Builder.toLazyText out)
  -- unless () $ do
  --
  --   debugStrings `forM_` \s ->
  --     trace s $ return ()

-- | Additionally checks that the output compiles again, appending an error
-- if it does not.
pPrintModuleAndCheck
  :: Config
  -> ExactPrint.Anns
  -> GHC.ParsedSource
  -> IO ([BrittanyError], TextL.Text)
pPrintModuleAndCheck conf anns parsedModule = do
  let ghcOptions     = conf & _conf_forward & _options_ghc & runIdentity
  let (errs, output) = pPrintModule conf anns parsedModule
  parseResult <- parseModuleFromString ghcOptions
                                       "output"
                                       (\_ -> return $ Right ())
                                       (TextL.unpack output)
  let errs' = errs ++ case parseResult of
        Left{}  -> [ErrorOutputCheck]
        Right{} -> []
  return (errs', output)


-- used for testing mostly, currently.
-- TODO: use parsePrintModule instead and remove this function.
parsePrintModuleTests :: Config -> String -> Text -> IO (Either String Text)
parsePrintModuleTests conf filename input = do
  let inputStr = Text.unpack input
  parseResult <- ExactPrint.Parsers.parseModuleFromString filename inputStr
  case parseResult of
    Left  (_   , s           ) -> return $ Left $ "parsing error: " ++ s
    Right (anns, parsedModule) -> do
      let omitCheck =
            conf
              &  _conf_errorHandling
              .> _econf_omit_output_valid_check
              .> confUnpack
      (errs, ltext) <- if omitCheck
        then return $ pPrintModule conf anns parsedModule
        else pPrintModuleAndCheck conf anns parsedModule
      return $ if null errs
        then Right $ TextL.toStrict $ ltext
        else
          let errStrs = errs <&> \case
                ErrorInput         str -> str
                ErrorUnusedComment str -> str
                LayoutWarning      str -> str
                ErrorUnknownNode str _ -> str
                ErrorOutputCheck       -> "Output is not syntactically valid."
          in  Left $ "pretty printing error(s):\n" ++ List.unlines errStrs


-- this approach would for with there was a pure GHC.parseDynamicFilePragma.
-- Unfortunately that does not exist yet, so we cannot provide a nominally
-- pure interface.

-- parsePrintModuleTests :: Text -> Either String Text
-- parsePrintModuleTests input = do
--   let dflags = GHC.unsafeGlobalDynFlags
--   let fakeFileName = "SomeTestFakeFileName.hs"
--   let pragmaInfo = GHC.getOptions
--         dflags
--         (GHC.stringToStringBuffer $ Text.unpack input)
--         fakeFileName
--   (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags pragmaInfo
--   let parseResult = ExactPrint.Parsers.parseWith
--         dflags1
--         fakeFileName
--         GHC.parseModule
--         inputStr
--   case parseResult of
--     Left (_, s) -> Left $ "parsing error: " ++ s
--     Right (anns, parsedModule) -> do
--       let (out, errs) = runIdentity
--                       $ runMultiRWSTNil
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiWriterAW
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiWriterW
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiReader anns
--                       $ ppModule parsedModule
--       if (not $ null errs)
--         then do
--           let errStrs = errs <&> \case
--                 ErrorUnusedComment str -> str
--           Left $ "pretty printing error(s):\n" ++ List.unlines errStrs
--         else return $ TextL.toStrict $ Text.Builder.toLazyText out

ppModule :: GenLocated SrcSpan (HsModule GhcPs) -> PPM ()
ppModule lmod@(L _loc _m@(HsModule _name _exports _ decls _ _)) = do
  post <- ppPreamble lmod
  decls `forM_` \decl -> do
    filteredAnns <- mAsk <&> \annMap ->
      Map.findWithDefault Map.empty (ExactPrint.mkAnnKey decl) annMap

    traceIfDumpConf "bridoc annotations filtered/transformed"
                    _dconf_dump_annotations
      $ annsDoc filteredAnns

    config <- mAsk

    MultiRWSS.withoutMultiReader $ do
      MultiRWSS.mPutRawR $ config :+: filteredAnns :+: HNil
      ppDecl decl
  let finalComments = filter
        ( fst .> \case
          ExactPrint.AnnComment{} -> True
          _                             -> False
        )
        post
  post `forM_` \case
    (ExactPrint.AnnComment (ExactPrint.Comment cmStr _ _), l) -> do
      ppmMoveToExactLoc l
      mTell $ Text.Builder.fromString cmStr
    (ExactPrint.G AnnEofPos, (ExactPrint.DP (eofZ, eofX))) ->
      let
        folder (acc, _) (kw, ExactPrint.DP (y, x)) = case kw of
          ExactPrint.AnnComment cm
            | GHC.RealSrcSpan span <- ExactPrint.commentIdentifier cm
            -> ( acc + y + GHC.srcSpanEndLine span - GHC.srcSpanStartLine span
               , x + GHC.srcSpanEndCol span - GHC.srcSpanStartCol span
               )
          _ -> (acc + y, x)
        (cmY, cmX) = foldl' folder (0, 0) finalComments
      in
        ppmMoveToExactLoc $ ExactPrint.DP (eofZ - cmY, eofX - cmX)
    _ -> return ()

withTransformedAnns :: Data ast => ast -> PPMLocal () -> PPMLocal ()
withTransformedAnns ast m = do
  -- TODO: implement `local` for MultiReader/MultiRWS
  readers@(conf :+: anns :+: HNil) <- MultiRWSS.mGetRawR
  MultiRWSS.mPutRawR (conf :+: f anns :+: HNil)
  m
  MultiRWSS.mPutRawR readers
 where
  f anns =
    let ((), (annsBalanced, _), _) =
          ExactPrint.runTransform anns (commentAnnFixTransformGlob ast)
    in  annsBalanced


ppDecl :: LHsDecl GhcPs -> PPMLocal ()
ppDecl d@(L loc decl) = case decl of
  SigD sig -> -- trace (_sigHead sig) $
              withTransformedAnns d $ do
    -- runLayouter $ Old.layoutSig (L loc sig)
    briDoc <- briDocMToPPM $ layoutSig (L loc sig)
    layoutBriDoc briDoc
  ValD bind -> -- trace (_bindHead bind) $
               withTransformedAnns d $ do
    -- Old.layoutBind (L loc bind)
    briDoc <- briDocMToPPM $ do
      eitherNode <- layoutBind (L loc bind)
      case eitherNode of
        Left  ns -> docLines $ return <$> ns
        Right n  -> return n
    layoutBriDoc briDoc
  _ -> briDocMToPPM (briDocByExactNoComment d) >>= layoutBriDoc

-- Prints the information associated with the module annotation
-- This includes the imports
ppPreamble :: GenLocated SrcSpan (HsModule GhcPs)
           -> PPM [(ExactPrint.KeywordId, ExactPrint.DeltaPos)]
ppPreamble lmod@(L loc m@(HsModule _ _ _ _ _ _)) = do
  filteredAnns <- mAsk <&> \annMap ->
    Map.findWithDefault Map.empty (ExactPrint.mkAnnKey lmod) annMap
    -- Since ghc-exactprint adds annotations following (implicit)
    -- modules to both HsModule and the elements in the module
    -- this can cause duplication of comments. So strip
    -- attached annotations that come after the module's where
    -- from the module node
  config <- mAsk
  let shouldReformatPreamble =
        config & _conf_layout & _lconfig_reformatModulePreamble & confUnpack

  let
    (filteredAnns', post) =
      case (ExactPrint.mkAnnKey lmod) `Map.lookup` filteredAnns of
        Nothing -> (filteredAnns, [])
        Just mAnn ->
          let
            modAnnsDp = ExactPrint.annsDP mAnn
            isWhere (ExactPrint.G AnnWhere) = True
            isWhere _                       = False
            isEof (ExactPrint.G AnnEofPos) = True
            isEof _                        = False
            whereInd     = List.findIndex (isWhere . fst) modAnnsDp
            eofInd       = List.findIndex (isEof . fst) modAnnsDp
            (pre, post') = case (whereInd, eofInd) of
              (Nothing, Nothing) -> ([], modAnnsDp)
              (Just i , Nothing) -> List.splitAt (i + 1) modAnnsDp
              (Nothing, Just _i) -> ([], modAnnsDp)
              (Just i , Just j ) -> List.splitAt (min (i + 1) j) modAnnsDp
            findInitialCommentSize = \case
              ((ExactPrint.AnnComment cm, ExactPrint.DP (y, _)) : rest) ->
                let GHC.RealSrcSpan span = ExactPrint.commentIdentifier cm
                in  y
                    + GHC.srcSpanEndLine span
                    - GHC.srcSpanStartLine span
                    + findInitialCommentSize rest
              _ -> 0
            initialCommentSize  = findInitialCommentSize pre
            fixAbsoluteModuleDP = \case
              (g@(ExactPrint.G AnnModule), ExactPrint.DP (y, x)) ->
                (g, ExactPrint.DP (y - initialCommentSize, x))
              x -> x
            pre' = if shouldReformatPreamble
              then map fixAbsoluteModuleDP pre
              else pre
            mAnn' = mAnn { ExactPrint.annsDP = pre' }
            filteredAnns'' =
              Map.insert (ExactPrint.mkAnnKey lmod) mAnn' filteredAnns
          in
            (filteredAnns'', post')
  traceIfDumpConf "bridoc annotations filtered/transformed"
                  _dconf_dump_annotations
    $ annsDoc filteredAnns'

  if shouldReformatPreamble
    then MultiRWSS.withoutMultiReader $ do
      MultiRWSS.mPutRawR $ config :+: filteredAnns' :+: HNil
      withTransformedAnns lmod $ do
        briDoc <- briDocMToPPM $ layoutModule lmod
        layoutBriDoc briDoc
    else
      let emptyModule = L loc m { hsmodDecls = [] }
      in  MultiRWSS.withMultiReader filteredAnns' $ processDefault emptyModule
  return post

_sigHead :: Sig GhcPs -> String
_sigHead = \case
  TypeSig names _ ->
    "TypeSig " ++ intercalate "," (Text.unpack . lrdrNameToText <$> names)
  _ -> "unknown sig"

_bindHead :: HsBind GhcPs -> String
_bindHead = \case
  FunBind fId _ _ _ [] -> "FunBind " ++ (Text.unpack $ lrdrNameToText $ fId)
  PatBind _pat _ _ _ ([], []) -> "PatBind smth"
  _ -> "unknown bind"



layoutBriDoc :: BriDocNumbered -> PPMLocal ()
layoutBriDoc briDoc = do
  -- first step: transform the briDoc.
  briDoc'                       <- MultiRWSS.withMultiStateS BDEmpty $ do
    -- Note that briDoc is BriDocNumbered, but state type is BriDoc.
    -- That's why the alt-transform looks a bit special here.
    traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw
      $ briDocToDoc
      $ unwrapBriDocNumbered
      $ briDoc
    -- bridoc transformation: remove alts
    transformAlts briDoc >>= mSet
    mGet >>= briDocToDoc .> traceIfDumpConf "bridoc post-alt"
                                            _dconf_dump_bridoc_simpl_alt
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyFloating .> mSet
    mGet >>= briDocToDoc .> traceIfDumpConf "bridoc post-floating"
                                            _dconf_dump_bridoc_simpl_floating
    -- bridoc transformation: par removal
    mGet >>= transformSimplifyPar .> mSet
    mGet >>= briDocToDoc .> traceIfDumpConf "bridoc post-par"
                                            _dconf_dump_bridoc_simpl_par
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyColumns .> mSet
    mGet >>= briDocToDoc .> traceIfDumpConf "bridoc post-columns"
                                            _dconf_dump_bridoc_simpl_columns
    -- bridoc transformation: indent
    mGet >>= transformSimplifyIndent .> mSet
    mGet >>= briDocToDoc .> traceIfDumpConf "bridoc post-indent"
                                            _dconf_dump_bridoc_simpl_indent
    mGet >>= briDocToDoc .> traceIfDumpConf "bridoc final"
                                            _dconf_dump_bridoc_final
    -- -- convert to Simple type
    -- simpl <- mGet <&> transformToSimple
    -- return simpl

  anns :: ExactPrint.Anns <- mAsk

  let state = LayoutState
        { _lstate_baseYs           = [0]
        , _lstate_curYOrAddNewline = Right 0 -- important that we use left here
                                             -- because moveToAnn stuff of the
                                             -- first node needs to do its
                                             -- thing properly.
        , _lstate_indLevels        = [0]
        , _lstate_indLevelLinger   = 0
        , _lstate_comments         = anns
        , _lstate_commentCol       = Nothing
        , _lstate_addSepSpace      = Nothing
        }

  state' <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'

  let remainingComments =
        extractAllComments =<< Map.elems (_lstate_comments state')
  remainingComments
    `forM_` (fst .> show .> ErrorUnusedComment .> (:[]) .> mTell)

  return $ ()
