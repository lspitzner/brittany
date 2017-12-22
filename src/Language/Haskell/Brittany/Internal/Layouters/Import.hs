module Language.Haskell.Brittany.Internal.Layouters.Import (layoutImport) where

#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
import           Language.Haskell.Brittany.Internal.Config.Types

import           RdrName                                  ( RdrName(..) )
import           GHC                                      ( unLoc
                                                          , GenLocated(L)
                                                          , moduleNameString
                                                          , Located
                                                          )
import           HsSyn
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
  ImportDecl _ (L _ modName) pkg src safe q False mas mllies -> do
    importCol <- mAsk <&> _conf_layout .> _lconfig_importColumn .> confUnpack
    indentPolicy <- mAsk <&>  _conf_layout .> _lconfig_indentPolicy .> confUnpack
    let
      compact  = indentPolicy == IndentPolicyLeft
      modNameT = Text.pack $ moduleNameString modName
      pkgNameT = Text.pack . prepPkg . sl_st <$> pkg
      masT     = Text.pack . moduleNameString . prepModName <$> mas
      hiding   = maybe False fst mllies
      minQLength = length "import qualified "
      qLengthReal =
        let qualifiedPart = if q then length "qualified " else 0
            safePart      = if safe then length "safe " else 0
            pkgPart       = maybe 0 ((+ 1) . Text.length) pkgNameT
            srcPart       = if src then length "{-# SOURCE #-} " else 0
        in  length "import " + srcPart + safePart + qualifiedPart + pkgPart
      qLength          = max minQLength qLengthReal
      -- Cost in columns of importColumn
      asCost           = length "as "
      bindingCost      = if hiding then length "hiding ( " else length "( "
      nameCost         = Text.length modNameT + qLength
      importQualifiers = docSeq
        [ appSep $ docLit $ Text.pack "import"
        , if src then appSep $ docLit $ Text.pack "{-# SOURCE #-}" else docEmpty
        , if safe then appSep $ docLit $ Text.pack "safe" else docEmpty
        , if q then appSep $ docLit $ Text.pack "qualified" else docEmpty
        , maybe docEmpty (appSep . docLit) pkgNameT
        ]
      indentName =
        if compact then id else docEnsureIndent (BrIndentSpecial qLength)
      modNameD =
        indentName $ appSep $ docLit modNameT
      hidDoc =
        if hiding then appSep $ docLit $ Text.pack "hiding" else docEmpty
      importHead = docSeq [importQualifiers, modNameD]
      bindingsD  = case mllies of
        Nothing -> docEmpty
        Just (_, llies) -> do
          hasComments <- hasAnyCommentsBelow llies
          if compact
          then docSeq [hidDoc, layoutLLIEs llies]
          else do
            ieDs <- layoutAnnAndSepLLIEs llies
            docWrapNodeRest llies $ case ieDs of
              -- ..[hiding].( )
              [] -> if hasComments
                then docPar
                  (docSeq [hidDoc, docParenLSep, docWrapNode llies docEmpty])
                  docParenR
                else docSeq [hidDoc, docParenLSep, docSeparator, docParenR]
                -- ..[hiding].( b )
              [ieD] -> if hasComments
                then docPar (docSeq [hidDoc, docParenLSep, ieD]) docParenR
                else docSeq [hidDoc, docParenLSep, ieD, docSeparator, docParenR]
                -- ..[hiding].( b
                --            , b'
                --            )
              (ieD:ieDs') ->
                docPar (docSeq [hidDoc, docSetBaseY $ docSeq [docParenLSep, ieD]])
                  $  docLines
                  $  ieDs'
                  ++ [docParenR]
      bindingLine =
        docEnsureIndent (BrIndentSpecial (importCol - bindingCost)) bindingsD
      makeAsDoc asT =
        docSeq [appSep $ docLit $ Text.pack "as", appSep $ docLit asT]
    if compact
    then
      let asDoc = maybe docEmpty makeAsDoc masT
      in docAlt
        [ docForceSingleline $
            docSeq [importHead, asDoc, docSetBaseY $ bindingsD]
        , docAddBaseY BrIndentRegular $
            docPar (docSeq [importHead, asDoc]) bindingsD
        ]
    else
      case masT of
        Just n | enoughRoom -> docLines [docSeq [importHead, asDoc], bindingLine]
               | otherwise  -> docLines [importHead, asDoc, bindingLine]
         where
          enoughRoom = nameCost < importCol - asCost
          asDoc =
            docEnsureIndent (BrIndentSpecial (importCol - asCost))
              $ makeAsDoc n
        Nothing | enoughRoom -> docSeq [importHead, bindingLine]
                | otherwise  -> docLines [importHead, bindingLine]
          where enoughRoom = nameCost < importCol - bindingCost
  _ -> docEmpty
