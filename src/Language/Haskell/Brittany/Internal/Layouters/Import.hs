module Language.Haskell.Brittany.Internal.Layouters.Import (layoutImport) where

#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
import           Language.Haskell.Brittany.Internal.Config.Types

import           GHC                                      ( unLoc
                                                          , GenLocated(L)
                                                          , moduleNameString
                                                          , Located
                                                          )
#if MIN_VERSION_ghc(8,10,1)   /* ghc-8.10.1 */
import           GHC.Hs
#else
import           HsSyn
#endif
import           Name
import           FieldLabel
import qualified FastString
import           BasicTypes

import           Language.Haskell.Brittany.Internal.Utils



#if MIN_VERSION_ghc(8,2,0)
prepPkg :: SourceText -> String
prepPkg rawN = case rawN of
  SourceText n -> n
  -- This would be odd to encounter and the
  -- result will most certainly be wrong
  NoSourceText -> ""
#else
prepPkg :: String -> String
prepPkg = id
#endif
#if MIN_VERSION_ghc(8,2,0)
prepModName :: Located e -> e
prepModName = unLoc
#else
prepModName :: e -> e
prepModName = id
#endif

layoutImport :: ToBriDoc ImportDecl
layoutImport limportD@(L _ importD) = docWrapNode limportD $ case importD of
#if MIN_VERSION_ghc(8,6,0)
  ImportDecl _ _ (L _ modName) pkg src safe q False mas mllies -> do
#else
  ImportDecl _ (L _ modName) pkg src safe q False mas mllies -> do
#endif
    importCol <- mAsk <&> _conf_layout .> _lconfig_importColumn .> confUnpack
    importAsCol <- mAsk <&> _conf_layout .> _lconfig_importAsColumn .> confUnpack
    indentPolicy <- mAsk <&>  _conf_layout .> _lconfig_indentPolicy .> confUnpack
    let
      compact  = indentPolicy /= IndentPolicyFree
      modNameT = Text.pack $ moduleNameString modName
      pkgNameT = Text.pack . prepPkg . sl_st <$> pkg
      masT     = Text.pack . moduleNameString . prepModName <$> mas
      hiding   = maybe False fst mllies
      minQLength = length "import qualified "
      qLengthReal =
#if MIN_VERSION_ghc(8,10,1)   /* ghc-8.10.1 */
        let qualifiedPart = if q /= NotQualified then length "qualified " else 0
#else
        let qualifiedPart = if q then length "qualified " else 0
#endif
            safePart      = if safe then length "safe " else 0
            pkgPart       = maybe 0 ((+ 1) . Text.length) pkgNameT
            srcPart       = if src then length "{-# SOURCE #-} " else 0
        in  length "import " + srcPart + safePart + qualifiedPart + pkgPart
      qLength          = max minQLength qLengthReal
      -- Cost in columns of importColumn
      asCost           = length "as "
      hidingParenCost  = if hiding then length "hiding ( " else length "( "
      nameCost         = Text.length modNameT + qLength
      importQualifiers = docSeq
        [ appSep $ docLit $ Text.pack "import"
        , if src then appSep $ docLit $ Text.pack "{-# SOURCE #-}" else docEmpty
        , if safe then appSep $ docLit $ Text.pack "safe" else docEmpty
#if MIN_VERSION_ghc(8,10,1)   /* ghc-8.10.1 */
        , if q /= NotQualified then appSep $ docLit $ Text.pack "qualified" else docEmpty
#else
        , if q then appSep $ docLit $ Text.pack "qualified" else docEmpty
#endif
        , maybe docEmpty (appSep . docLit) pkgNameT
        ]
      indentName =
        if compact then id else docEnsureIndent (BrIndentSpecial qLength)
      modNameD =
        indentName $ appSep $ docLit modNameT
      hidDocCol = if hiding then importCol - hidingParenCost else importCol - 2
      hidDocColDiff = importCol - 2 - hidDocCol
      hidDoc = if hiding
        then appSep $ docLit $ Text.pack "hiding"
        else docEmpty
      importHead = docSeq [importQualifiers, modNameD]
      bindingsD  = case mllies of
        Nothing -> docEmpty
        Just (_, llies) -> do
          hasComments <- hasAnyCommentsBelow llies
          if compact
          then docAlt
            [ docSeq [hidDoc, docForceSingleline $ layoutLLIEs True llies]
            , let makeParIfHiding = if hiding
                    then docAddBaseY BrIndentRegular . docPar hidDoc
                    else id
              in makeParIfHiding (layoutLLIEs True llies)
            ]
          else do
            ieDs <- layoutAnnAndSepLLIEs llies
            docWrapNodeRest llies
              $ docEnsureIndent (BrIndentSpecial hidDocCol)
              $ case ieDs of
                -- ..[hiding].( )
                [] -> if hasComments
                  then docPar
                    (docSeq [hidDoc, docParenLSep, docWrapNode llies docEmpty])
                    (docEnsureIndent (BrIndentSpecial hidDocColDiff) docParenR)
                  else docSeq [hidDoc, docParenLSep, docSeparator, docParenR]
                -- ..[hiding].( b )
                [ieD] -> runFilteredAlternative $ do
                  addAlternativeCond (not hasComments)
                    $ docSeq
                    [ hidDoc
                    , docParenLSep
                    , docForceSingleline ieD
                    , docSeparator
                    , docParenR
                    ]
                  addAlternative $ docPar
                    (docSeq [hidDoc, docParenLSep, docNonBottomSpacing ieD])
                    (docEnsureIndent (BrIndentSpecial hidDocColDiff) docParenR)
                -- ..[hiding].( b
                --            , b'
                --            )
                (ieD:ieDs') ->
                  docPar
                    (docSeq [hidDoc, docSetBaseY $ docSeq [docParenLSep, ieD]])
                    (  docEnsureIndent (BrIndentSpecial hidDocColDiff)
                    $  docLines
                    $  ieDs'
                    ++ [docParenR]
                    )
      makeAsDoc asT =
        docSeq [appSep $ docLit $ Text.pack "as", appSep $ docLit asT]
    if compact
    then
      let asDoc = maybe docEmpty makeAsDoc masT
      in docAlt
        [ docForceSingleline $ docSeq [importHead, asDoc, bindingsD]
        , docAddBaseY BrIndentRegular $
            docPar (docSeq [importHead, asDoc]) bindingsD
        ]
    else
      case masT of
        Just n -> if enoughRoom
          then docLines
                 [ docSeq [importHead, asDoc], bindingsD]
          else docLines [importHead, asDoc, bindingsD]
         where
          enoughRoom = nameCost < importAsCol - asCost
          asDoc =
            docEnsureIndent (BrIndentSpecial (importAsCol - asCost))
              $ makeAsDoc n
        Nothing -> if enoughRoom
          then docSeq [importHead, bindingsD]
          else docLines [importHead, bindingsD]
          where enoughRoom = nameCost < importCol - hidingParenCost
  _ -> docEmpty
