module Language.Haskell.Brittany.Internal.Layouters.Import (layoutImport) where

#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
import           Language.Haskell.Brittany.Internal.Config.Types

import           RdrName (RdrName(..))
import GHC (unLoc, runGhc, GenLocated(L), moduleNameString, AnnKeywordId(..))
import           HsSyn
import           Name
import           HsImpExp
import           FieldLabel
import qualified FastString
import           BasicTypes

import           Language.Haskell.Brittany.Internal.Utils

layoutImport :: ToBriDoc ImportDecl
layoutImport limportD@(L _ importD) = docWrapNode limportD $ case importD of
  ImportDecl _ (L _ modName) pkg src safe q False as llies ->
    let
      modNameT         = Text.pack $ moduleNameString modName
      pkgNameT         = Text.pack . sl_st <$> pkg
      asT              = Text.pack . moduleNameString <$> as
      sig              = ColBindingLine (Just (Text.pack "import"))
      importQualifiers = docSeq
        [ appSep $ docLit $ Text.pack "import"
        , if src then appSep $ docLit $ Text.pack "{-# SOURCE #-}" else docEmpty
        , if safe then appSep $ docLit $ Text.pack "safe" else docEmpty
        , if q then appSep $ docLit $ Text.pack "qualified" else docEmpty
        , fromMaybe docEmpty (appSep . docLit <$> pkgNameT)
        ]
      makeAs asT' =
        appSep $ docSeq [docLit (Text.pack "as"), docSeparator, docLit asT']
      importIds =
        docSeq $ [appSep $ docLit modNameT, fromMaybe docEmpty (makeAs <$> asT)]
    in
      do
        (hiding, ies) <- case llies of
          Just (h, L _ lies) -> do
            sies <- docSharedWrapper layoutIEList lies
            return (h, sies)
          Nothing -> return (False, docEmpty)
        h <- docSharedWrapper
          ( const
            ( docSeq
              [ docCols sig [importQualifiers, importIds]
              , if hiding
                then appSep $ docLit $ Text.pack "hiding"
                else docEmpty
              ]
            )
          )
          ()
        docAlt
          [ docSeq [h, docForceSingleline ies]
          , docAddBaseY BrIndentRegular $ docPar h (docForceMultiline ies)
          ]
  _ -> docEmpty
