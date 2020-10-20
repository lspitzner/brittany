module Language.Haskell.Brittany.Internal.Layouters.Module (layoutModule) where

#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
import           Language.Haskell.Brittany.Internal.Layouters.Import
import           Language.Haskell.Brittany.Internal.Config.Types

import GHC (unLoc, runGhc, GenLocated(L), moduleNameString, AnnKeywordId(..))
import           HsSyn
import           Name
import           HsImpExp
import           FieldLabel
import qualified FastString
import           BasicTypes
import           Language.Haskell.GHC.ExactPrint as ExactPrint
import           Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types

import           Language.Haskell.Brittany.Internal.Utils



layoutModule :: ToBriDoc HsModule
layoutModule lmod@(L _ mod') = case mod' of
    -- Implicit module Main
  HsModule Nothing  _   imports _ _ _ -> docLines $ map layoutImport imports
  HsModule (Just n) les imports _ _ _ -> do
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
                  Just x  -> layoutLLIEs True x
                , docSeparator
                , docLit $ Text.pack "where"
                ]
            addAlternative
              $ docLines
              [ docAddBaseY BrIndentRegular $ docPar
                (docSeq [appSep $ docLit $ Text.pack "module", docLit tn]
                )
                (docSeq [ docWrapNode lmod $ case les of
                            Nothing -> docEmpty
                            Just x  -> layoutLLIEs False x
                        , docSeparator
                        , docLit $ Text.pack "where"
                        ]
                )
              ]
          ]
      : map layoutImport imports
