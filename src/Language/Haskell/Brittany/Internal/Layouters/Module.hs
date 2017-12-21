module Language.Haskell.Brittany.Internal.Layouters.Module (layoutModule) where

#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
import           Language.Haskell.Brittany.Internal.Layouters.Import
import           Language.Haskell.Brittany.Internal.Config.Types

import           RdrName (RdrName(..))
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
layoutModule lmod@(L _ mod') = do
  case mod' of
    -- Implicit module Main
    HsModule Nothing  _   imports _ _ _ -> docLines $ map layoutImport imports
    HsModule (Just n) les imports _ _ _ -> do
      let tn = Text.pack $ moduleNameString $ unLoc n
      (hasComments, exportsDoc) <- case les of
        Nothing    -> return (False, docEmpty)
        Just llies -> do
          hasComments <- hasAnyCommentsBelow llies
          exportsDoc  <- docSharedWrapper layoutLLIEs llies
          return (hasComments, exportsDoc)
      docLines
        $ docSeq
            [ docWrapNode lmod $ docEmpty
               -- A pseudo node that serves merely to force documentation
               -- before the node
            , docAlt
              (  [ docSeq
                     [ appSep $ docLit $ Text.pack "module"
                     , appSep $ docLit tn
                     , appSep $ docForceSingleline exportsDoc
                     , docLit $ Text.pack "where"
                     ]
                 | not hasComments
                 ]
              ++ [ docLines
                     [ docAddBaseY BrIndentRegular $ docPar
                       ( docSeq
                         [appSep $ docLit $ Text.pack "module", docLit tn]
                       )
                       (docForceMultiline exportsDoc)
                     , docLit $ Text.pack "where"
                     ]
                 ]
              )
            ]
        : map layoutImport imports
