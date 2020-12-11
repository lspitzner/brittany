{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal
  ( parsePrintModule
  , parsePrintModuleTests
  , pPrintModule
  , pPrintModuleAndCheck
   -- re-export from utils:
  , parseModule
  , parseModuleFromString
  , extractCommentConfigs
  , getTopLevelDeclNameMap
  )
where



#include "prelude.inc"

-- brittany { lconfig_importAsColumn: 60, lconfig_importColumn: 60 }
import qualified Language.Haskell.GHC.ExactPrint         as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types   as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint.Parsers

import           Data.Data
import           Control.Monad.Trans.Except
import           Data.HList.HList
import qualified Data.Yaml
import qualified Data.ByteString.Char8
import           Data.CZipWith
import qualified UI.Butcher.Monadic                      as Butcher

import qualified Data.Text.Lazy.Builder                  as Text.Builder

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

import qualified GHC                           as GHC
                                                   hiding ( parseModule )
import           ApiAnnotation                            ( AnnKeywordId(..) )
import           GHC                                      ( Located
                                                          , runGhc
                                                          , GenLocated(L)
                                                          , moduleNameString
                                                          )
import           RdrName                                  ( RdrName(..) )
import           SrcLoc                                   ( SrcSpan )
#if MIN_VERSION_ghc(8,10,1)   /* ghc-8.10.1 */
import           GHC.Hs
import           Bag
#else
import           HsSyn
#endif
import qualified DynFlags                                as GHC
import qualified GHC.LanguageExtensions.Type             as GHC

import           Data.Char                                ( isSpace )



data InlineConfigTarget
    = InlineConfigTargetModule
    | InlineConfigTargetNextDecl    -- really only next in module
    | InlineConfigTargetNextBinding -- by name
    | InlineConfigTargetBinding String

extractCommentConfigs
  :: ExactPrint.Anns
  -> TopLevelDeclNameMap
  -> Either (String, String) (CConfig Option, PerItemConfig)
extractCommentConfigs anns (TopLevelDeclNameMap declNameMap) = do
  let
    commentLiness =
      [ ( k
        , [ x
          | (ExactPrint.Comment x _ _, _) <-
            (  ExactPrint.annPriorComments ann
            ++ ExactPrint.annFollowingComments ann
            )
          ]
          ++ [ x
             | (ExactPrint.AnnComment (ExactPrint.Comment x _ _), _) <-
               ExactPrint.annsDP ann
             ]
        )
      | (k, ann) <- Map.toList anns
      ]
  let configLiness = commentLiness <&> second
        (Data.Maybe.mapMaybe $ \line -> do
          l1 <-
            List.stripPrefix "-- BRITTANY" line
            <|> List.stripPrefix "--BRITTANY" line
            <|> List.stripPrefix "-- brittany" line
            <|> List.stripPrefix "--brittany" line
            <|> (List.stripPrefix "{- BRITTANY" line >>= stripSuffix "-}")
          let l2 = dropWhile isSpace l1
          guard
            (  ("@" `isPrefixOf` l2)
            || ("-disable" `isPrefixOf` l2)
            || ("-next" `isPrefixOf` l2)
            || ("{" `isPrefixOf` l2)
            || ("--" `isPrefixOf` l2)
            )
          pure l2
        )
  let
    configParser = Butcher.addAlternatives
      [ ( "commandline-config"
        , \s -> "-" `isPrefixOf` dropWhile (== ' ') s
        , cmdlineConfigParser
        )
      , ( "yaml-config-document"
        , \s -> "{" `isPrefixOf` dropWhile (== ' ') s
        , Butcher.addCmdPart (Butcher.varPartDesc "yaml-config-document")
        $ fmap (\lconf -> (mempty { _conf_layout = lconf }, ""))
        . either (\_ -> Nothing) Just
        . Data.Yaml.decodeEither'
        . Data.ByteString.Char8.pack
          -- TODO: use some proper utf8 encoder instead?
        )
      ]
    parser = do -- we will (mis?)use butcher here to parse the inline config
                -- line.
      let nextDecl = do
            conf <- configParser
            Butcher.addCmdImpl (InlineConfigTargetNextDecl, conf)
      Butcher.addCmd "-next-declaration" nextDecl
      Butcher.addCmd "-Next-Declaration" nextDecl
      Butcher.addCmd "-NEXT-DECLARATION" nextDecl
      let nextBinding = do
            conf <- configParser
            Butcher.addCmdImpl (InlineConfigTargetNextBinding, conf)
      Butcher.addCmd "-next-binding" nextBinding
      Butcher.addCmd "-Next-Binding" nextBinding
      Butcher.addCmd "-NEXT-BINDING" nextBinding
      let disableNextBinding = do
            Butcher.addCmdImpl
              ( InlineConfigTargetNextBinding
              , mempty { _conf_roundtrip_exactprint_only = pure $ pure True }
              )
      Butcher.addCmd "-disable-next-binding" disableNextBinding
      Butcher.addCmd "-Disable-Next-Binding" disableNextBinding
      Butcher.addCmd "-DISABLE-NEXT-BINDING" disableNextBinding
      let disableNextDecl = do
            Butcher.addCmdImpl
              ( InlineConfigTargetNextDecl
              , mempty { _conf_roundtrip_exactprint_only = pure $ pure True }
              )
      Butcher.addCmd "-disable-next-declaration" disableNextDecl
      Butcher.addCmd "-Disable-Next-Declaration" disableNextDecl
      Butcher.addCmd "-DISABLE-NEXT-DECLARATION" disableNextDecl
      let disableFormatting = do
            Butcher.addCmdImpl
              ( InlineConfigTargetModule
              , mempty { _conf_disable_formatting = pure $ pure True }
              )
      Butcher.addCmd "-disable" disableFormatting
      Butcher.addCmd "@" $ do
        -- Butcher.addCmd "module" $ do
        --   conf <- configParser
        --   Butcher.addCmdImpl (InlineConfigTargetModule, conf)
        Butcher.addNullCmd $ do
          bindingName <- Butcher.addParamString "BINDING" mempty
          conf        <- configParser
          Butcher.addCmdImpl (InlineConfigTargetBinding bindingName, conf)
      conf <- configParser
      Butcher.addCmdImpl (InlineConfigTargetModule, conf)
  lineConfigss <- configLiness `forM` \(k, ss) -> do
    r <- ss `forM` \s -> case Butcher.runCmdParserSimple s parser of
      Left  err -> Left $ (err, s)
      Right c   -> Right $ c
    pure (k, r)

  let perModule = foldl'
        (<>)
        mempty
        [ conf
        | (_                       , lineConfigs) <- lineConfigss
        , (InlineConfigTargetModule, conf       ) <- lineConfigs
        ]
  let
    perBinding = Map.fromListWith
      (<>)
      [ (n, conf)
      | (k     , lineConfigs) <- lineConfigss
      , (target, conf       ) <- lineConfigs
      , n                     <- case target of
        InlineConfigTargetBinding s -> [s]
        InlineConfigTargetNextBinding | Just name <- Map.lookup k declNameMap ->
          [name]
        _ -> []
      ]
  let
    perKey = Map.fromListWith
      (<>)
      [ (k, conf)
      | (k     , lineConfigs) <- lineConfigss
      , (target, conf       ) <- lineConfigs
      , case target of
        InlineConfigTargetNextDecl -> True
        InlineConfigTargetNextBinding | Nothing <- Map.lookup k declNameMap ->
          True
        _ -> False
      ]

  pure
    $ ( perModule
      , PerItemConfig { _icd_perBinding = perBinding, _icd_perKey = perKey }
      )


getTopLevelDeclNameMap :: GHC.ParsedSource -> TopLevelDeclNameMap
getTopLevelDeclNameMap (L _ (HsModule _name _exports _ decls _ _)) =
  TopLevelDeclNameMap $ Map.fromList
    [ (ExactPrint.mkAnnKey decl, name)
    | decl       <- decls
    , (name : _) <- [getDeclBindingNames decl]
    ]


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
parsePrintModule configWithDebugs inputText = runExceptT $ do
  let config =
        configWithDebugs { _conf_debug = _conf_debug staticDefaultConfig }
  let ghcOptions         = config & _conf_forward & _options_ghc & runIdentity
  let config_pp          = config & _conf_preprocessor
  let cppMode            = config_pp & _ppconf_CPPMode & confUnpack
  let hackAroundIncludes = config_pp & _ppconf_hackAroundIncludes & confUnpack
  (anns, parsedSource, hasCPP) <- do
    let hackF s = if "#include" `isPrefixOf` s
          then "-- BRITANY_INCLUDE_HACK " ++ s
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
  (inlineConf, perItemConf) <-
    either (throwE . (: []) . uncurry ErrorMacroConfig) pure
      $ extractCommentConfigs anns (getTopLevelDeclNameMap parsedSource)
  let moduleConfig      = cZipWith fromOptionIdentity config inlineConf
  let disableFormatting = moduleConfig & _conf_disable_formatting & confUnpack
  if disableFormatting
    then do
      return inputText
    else do
      (errsWarns, outputTextL) <- do
        let omitCheck =
              moduleConfig
                & _conf_errorHandling
                & _econf_omit_output_valid_check
                & confUnpack
        (ews, outRaw) <- if hasCPP || omitCheck
          then return $ pPrintModule moduleConfig perItemConf anns parsedSource
          else lift
            $ pPrintModuleAndCheck moduleConfig perItemConf anns parsedSource
        let hackF s = fromMaybe s
              $ TextL.stripPrefix (TextL.pack "-- BRITANY_INCLUDE_HACK ") s
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
          customErrOrder ErrorMacroConfig{}   = 5
      let hasErrors =
            case
                moduleConfig & _conf_errorHandling & _econf_Werror & confUnpack
              of
                False -> 0 < maximum (-1 : fmap customErrOrder errsWarns)
                True  -> not $ null errsWarns
      if hasErrors
        then throwE $ errsWarns
        else pure $ TextL.toStrict outputTextL



-- BrittanyErrors can be non-fatal warnings, thus both are returned instead
-- of an Either.
-- This should be cleaned up once it is clear what kinds of errors really
-- can occur.
pPrintModule
  :: Config
  -> PerItemConfig
  -> ExactPrint.Anns
  -> GHC.ParsedSource
  -> ([BrittanyError], TextL.Text)
pPrintModule conf inlineConf anns parsedModule =
  let ((out, errs), debugStrings) =
        runIdentity
          $ MultiRWSS.runMultiRWSTNil
          $ MultiRWSS.withMultiWriterAW
          $ MultiRWSS.withMultiWriterAW
          $ MultiRWSS.withMultiWriterW
          $ MultiRWSS.withMultiReader anns
          $ MultiRWSS.withMultiReader conf
          $ MultiRWSS.withMultiReader inlineConf
          $ MultiRWSS.withMultiReader (extractToplevelAnns parsedModule anns)
          $ do
              traceIfDumpConf "bridoc annotations raw" _dconf_dump_annotations
                $ annsDoc anns
              ppModule parsedModule
      tracer = if Seq.null debugStrings
        then id
        else
          trace ("---- DEBUGMESSAGES ---- ")
            . foldr (seq . join trace) id debugStrings
  in  tracer $ (errs, Text.Builder.toLazyText out)
  -- unless () $ do
  --
  --   debugStrings `forM_` \s ->
  --     trace s $ return ()

-- | Additionally checks that the output compiles again, appending an error
-- if it does not.
pPrintModuleAndCheck
  :: Config
  -> PerItemConfig
  -> ExactPrint.Anns
  -> GHC.ParsedSource
  -> IO ([BrittanyError], TextL.Text)
pPrintModuleAndCheck conf inlineConf anns parsedModule = do
  let ghcOptions     = conf & _conf_forward & _options_ghc & runIdentity
  let (errs, output) = pPrintModule conf inlineConf anns parsedModule
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
#if MIN_VERSION_ghc(8,10,1)   /* ghc-8.10.1 */
    Left  err                  -> return $ Left $ "parsing error: " ++ show (bagToList (show <$> err))
#else
    Left  (_   , s           ) -> return $ Left $ "parsing error: " ++ s
#endif
    Right (anns, parsedModule) -> runExceptT $ do
      (inlineConf, perItemConf) <-
        case extractCommentConfigs anns (getTopLevelDeclNameMap parsedModule) of
          Left  err -> throwE $ "error in inline config: " ++ show err
          Right x   -> pure x
      let moduleConf = cZipWith fromOptionIdentity conf inlineConf
      let omitCheck =
            conf
              &  _conf_errorHandling
              .> _econf_omit_output_valid_check
              .> confUnpack
      (errs, ltext) <- if omitCheck
        then return $ pPrintModule moduleConf perItemConf anns parsedModule
        else lift
          $ pPrintModuleAndCheck moduleConf perItemConf anns parsedModule
      if null errs
        then pure $ TextL.toStrict $ ltext
        else
          let
            errStrs = errs <&> \case
              ErrorInput         str -> str
              ErrorUnusedComment str -> str
              LayoutWarning      str -> str
              ErrorUnknownNode str _ -> str
              ErrorMacroConfig str _ -> "when parsing inline config: " ++ str
              ErrorOutputCheck       -> "Output is not syntactically valid."
          in  throwE $ "pretty printing error(s):\n" ++ List.unlines errStrs


-- this approach would for if there was a pure GHC.parseDynamicFilePragma.
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

toLocal :: Config -> ExactPrint.Anns -> PPMLocal a -> PPM a
toLocal conf anns m = do
  (x, write) <-
    lift $ MultiRWSS.runMultiRWSTAW (conf :+: anns :+: HNil) HNil $ m
  MultiRWSS.mGetRawW >>= \w -> MultiRWSS.mPutRawW (w `mappend` write)
  pure x

ppModule :: GenLocated SrcSpan (HsModule GhcPs) -> PPM ()
ppModule lmod@(L _loc _m@(HsModule _name _exports _ decls _ _)) = do
  post <- ppPreamble lmod
  decls `forM_` \decl -> do
    let declAnnKey       = ExactPrint.mkAnnKey decl
    let declBindingNames = getDeclBindingNames decl
    inlineConf <- mAsk
    let mDeclConf = Map.lookup declAnnKey $ _icd_perKey inlineConf
    let mBindingConfs =
          declBindingNames <&> \n -> Map.lookup n $ _icd_perBinding inlineConf
    filteredAnns <- mAsk
      <&> \annMap -> Map.findWithDefault Map.empty declAnnKey annMap

    traceIfDumpConf "bridoc annotations filtered/transformed"
                    _dconf_dump_annotations
      $ annsDoc filteredAnns

    config <- mAsk

    let config' = cZipWith fromOptionIdentity config
          $ mconcat (catMaybes (mBindingConfs ++ [mDeclConf]))

    let exactprintOnly = config' & _conf_roundtrip_exactprint_only & confUnpack
    toLocal config' filteredAnns $ do
      bd <- if exactprintOnly
        then briDocMToPPM $ briDocByExactNoComment decl
        else do
          (r, errs, debugs) <- briDocMToPPMInner $ layoutDecl decl
          mTell debugs
          mTell errs
          if null errs
            then pure r
            else briDocMToPPM $ briDocByExactNoComment decl
      layoutBriDoc bd

  let finalComments = filter
        (fst .> \case
          ExactPrint.AnnComment{} -> True
          _                       -> False
        )
        post
  post `forM_` \case
    (ExactPrint.AnnComment (ExactPrint.Comment cmStr _ _), l) -> do
      ppmMoveToExactLoc l
      mTell $ Text.Builder.fromString cmStr
    (ExactPrint.G AnnEofPos, (ExactPrint.DP (eofZ, eofX))) ->
      let folder (acc, _) (kw, ExactPrint.DP (y, x)) = case kw of
            ExactPrint.AnnComment cm
              | GHC.RealSrcSpan span <- ExactPrint.commentIdentifier cm
              -> ( acc + y + GHC.srcSpanEndLine span - GHC.srcSpanStartLine span
                 , x + GHC.srcSpanEndCol span - GHC.srcSpanStartCol span
                 )
            _ -> (acc + y, x)
          (cmY, cmX) = foldl' folder (0, 0) finalComments
      in  ppmMoveToExactLoc $ ExactPrint.DP (eofZ - cmY, eofX - cmX)
    _ -> return ()

getDeclBindingNames :: LHsDecl GhcPs -> [String]
getDeclBindingNames (L _ decl) = case decl of
  SigD _ (TypeSig _ ns _) -> ns <&> \(L _ n) -> Text.unpack (rdrNameToText n)
  ValD _ (FunBind _ (L _ n) _ _ _) -> [Text.unpack $ rdrNameToText n]
  _                              -> []


-- Prints the information associated with the module annotation
-- This includes the imports
ppPreamble
  :: GenLocated SrcSpan (HsModule GhcPs)
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
      case Map.lookup (ExactPrint.mkAnnKey lmod) filteredAnns of
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
            mAnn' = mAnn { ExactPrint.annsDP = pre }
            filteredAnns'' =
              Map.insert (ExactPrint.mkAnnKey lmod) mAnn' filteredAnns
          in
            (filteredAnns'', post')
  traceIfDumpConf "bridoc annotations filtered/transformed"
                  _dconf_dump_annotations
    $ annsDoc filteredAnns'

  if shouldReformatPreamble
    then toLocal config filteredAnns' $ withTransformedAnns lmod $ do
      briDoc <- briDocMToPPM $ layoutModule lmod
      layoutBriDoc briDoc
    else
      let emptyModule = L loc m { hsmodDecls = [] }
      in  MultiRWSS.withMultiReader filteredAnns' $ processDefault emptyModule
  return post

_sigHead :: Sig GhcPs -> String
_sigHead = \case
  TypeSig _ names _ ->
    "TypeSig " ++ intercalate "," (Text.unpack . lrdrNameToText <$> names)
  _ -> "unknown sig"

_bindHead :: HsBind GhcPs -> String
_bindHead = \case
  FunBind _ fId _ _ [] -> "FunBind " ++ (Text.unpack $ lrdrNameToText $ fId)
  PatBind _ _pat _ ([], []) -> "PatBind smth"
  _                           -> "unknown bind"



layoutBriDoc :: BriDocNumbered -> PPMLocal ()
layoutBriDoc briDoc = do
  -- first step: transform the briDoc.
  briDoc' <- MultiRWSS.withMultiStateS BDEmpty $ do
    -- Note that briDoc is BriDocNumbered, but state type is BriDoc.
    -- That's why the alt-transform looks a bit special here.
    traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw
      $ briDocToDoc
      $ unwrapBriDocNumbered
      $ briDoc
    -- bridoc transformation: remove alts
    transformAlts briDoc >>= mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-alt" _dconf_dump_bridoc_simpl_alt
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyFloating .> mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-floating"
                          _dconf_dump_bridoc_simpl_floating
    -- bridoc transformation: par removal
    mGet >>= transformSimplifyPar .> mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-par" _dconf_dump_bridoc_simpl_par
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyColumns .> mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-columns" _dconf_dump_bridoc_simpl_columns
    -- bridoc transformation: indent
    mGet >>= transformSimplifyIndent .> mSet
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc post-indent" _dconf_dump_bridoc_simpl_indent
    mGet
      >>= briDocToDoc
      .>  traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final
    -- -- convert to Simple type
    -- simpl <- mGet <&> transformToSimple
    -- return simpl

  anns :: ExactPrint.Anns <- mAsk

  let state = LayoutState { _lstate_baseYs           = [0]
                          , _lstate_curYOrAddNewline = Right 0 -- important that we dont use left
                                             -- here because moveToAnn stuff
                                             -- of the first node needs to do
                                             -- its thing properly.
                          , _lstate_indLevels        = [0]
                          , _lstate_indLevelLinger   = 0
                          , _lstate_comments         = anns
                          , _lstate_commentCol       = Nothing
                          , _lstate_addSepSpace      = Nothing
                          , _lstate_commentNewlines  = 0
                          }

  state' <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'

  let remainingComments =
        [ c
        | (ExactPrint.AnnKey _ con, elemAnns) <- Map.toList
          (_lstate_comments state')
          -- With the new import layouter, we manually process comments
          -- without relying on the backend to consume the comments out of
          -- the state/map. So they will end up here, and we need to ignore
          -- them.
        , ExactPrint.unConName con /= "ImportDecl"
        , c <- extractAllComments elemAnns
        ]
  remainingComments
    `forM_` (fst .> show .> ErrorUnusedComment .> (: []) .> mTell)

  return $ ()
