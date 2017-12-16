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
    HsModule Nothing _ imports _ _ _ -> docLines $ map layoutImport imports
    HsModule (Just n) les imports _ _ _ ->
      let
        tn = Text.pack $ moduleNameString $ unLoc n
      in
        do
          cs <- do
            anns <- mAsk
            case ExactPrint.Types.mkAnnKey lmod `Map.lookup` anns of
              Just mAnn -> return $ extractAllComments mAnn
              Nothing   -> return []
          (hasComments, es) <- case les of
            Nothing               -> return (False, docEmpty)
            Just llies@(L _ lies) -> do
              hasComments <- hasAnyCommentsBelow llies
              return (hasComments, docWrapNode llies $ layoutIEList lies)
          docLines
            (  [ -- A pseudo node that serves merely to force documentation
               -- before the node
                 docWrapNode lmod $ docEmpty
               | [] /= cs
               ]
            ++ [ docAlt
                   (  [ docSeq
                          [ appSep $ docLit $ Text.pack "module"
                          , appSep $ docLit tn
                          , appSep $ docForceSingleline es
                          , docLit $ Text.pack "where"
                          ]
                      | not hasComments
                      ]
                   ++ [ docLines
                          [ docAddBaseY BrIndentRegular $ docPar
                            ( docSeq
                              [ appSep $ docLit $ Text.pack "module"
                              , docLit tn
                              ]
                            )
                            (docForceMultiline es)
                          , docLit $ Text.pack "where"
                          ]
                      ]
                   )
               ]
            ++ map layoutImport imports
            )
