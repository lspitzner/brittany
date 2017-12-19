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
  ImportDecl _ (L _ modName) pkg src safe q False as mllies -> do
    importCol <- mAsk <&> _conf_layout .> _lconfig_importColumn .> confUnpack
    let
      modNameT        = Text.pack $ moduleNameString modName
      pkgNameT        = Text.pack . prepPkg . sl_st <$> pkg
      asT             = Text.pack . moduleNameString . prepModName <$> as
      (hiding, mlies) = case mllies of
        Just (h, L _ lies') -> (h, Just lies')
        Nothing             -> (False, Nothing)
      minQLength = length "import qualified "
      qLengthReal =
        let qualifiedPart = if q then length "qualified " else 0
            safePart      = if safe then length "safe " else 0
            pkgPart       = fromMaybe 0 ((+ 1) . Text.length <$> pkgNameT)
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
        , fromMaybe docEmpty (appSep . docLit <$> pkgNameT)
        ]
      modNameD =
        docEnsureIndent (BrIndentSpecial qLength) $ appSep $ docLit modNameT
      hidDoc =
        if hiding then appSep $ docLit $ Text.pack "hiding" else docEmpty
      importHead      = docSeq [importQualifiers, modNameD]
      Just lies       = mlies
      Just (_, llies) = mllies
      (ieH:ieT)       = map layoutIE lies
      makeIENode ie = docSeq [docCommaSep, ie]
      bindings@(bindingsH:bindingsT) =
        docSeq [docParenLSep, ieH]
          :  bindingsT'
          ++ [docSeq [docSeparator, docParenR]]
        where
          -- Handle the last element with docWrapNode llies
          bindingsT' =
            case ieT of
              [] -> []
              [ie] -> [makeIENode $ docWrapNode llies $ ie]
              _ -> map makeIENode (List.init ieT) ++ [makeIENode $ docWrapNode llies $ List.last ieT]
      bindingsD = case mlies of
        Nothing -> docSeq [docEmpty]
        -- ..[hiding].( )
        Just [] -> do
          hasComments <- hasAnyCommentsBelow llies
          if hasComments
          then
            docWrapNodeRest llies
            $ docPar
                (docSeq [hidDoc, docParenLSep, docWrapNode llies docEmpty])
            $ docLines [docParenR]
          else
            docWrapNodeRest llies $ docSeq [hidDoc, docParenLSep, docParenR]
        -- ..[hiding].( b )
        Just [_] -> do
          hasComments <- hasAnyCommentsBelow llies
          if hasComments
          then
            docWrapNodeRest llies $ docPar (docSeq [hidDoc, docWrapNode llies $ bindingsH]) $ docLines
              bindingsT
          else
            docWrapNodeRest llies $ docSeq $ hidDoc : bindings
        -- ..[hiding].( b
        --            , b'
        --            )
        Just _ ->
          docWrapNodeRest llies $ docPar (docSeq [hidDoc, docSetBaseY $ bindingsH])
            $ docLines bindingsT
      bindingLine =
        docEnsureIndent (BrIndentSpecial (importCol - bindingCost)) bindingsD
    case asT of
      Just n | enoughRoom -> docLines [docSeq [importHead, asDoc], bindingLine]
             | otherwise  -> docLines [importHead, asDoc, bindingLine]
       where
        enoughRoom = nameCost < importCol - asCost
        asDoc =
          docEnsureIndent (BrIndentSpecial (importCol - asCost))
            $ docSeq
            $ [appSep $ docLit $ Text.pack "as", docLit n]
      Nothing | enoughRoom -> docSeq [importHead, bindingLine]
              | otherwise  -> docLines [importHead, bindingLine]
        where enoughRoom = nameCost < importCol - bindingCost
  _ -> docEmpty
