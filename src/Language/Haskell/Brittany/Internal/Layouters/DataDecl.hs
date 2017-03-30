{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl
  ( layoutDataDecl
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Config.Types

import           RdrName ( RdrName(..) )
import           GHC ( Located, runGhc, GenLocated(L), moduleNameString )
import qualified GHC
import           HsSyn
import           Name
import           Language.Haskell.GHC.ExactPrint.Types ( mkAnnKey )

import           Language.Haskell.Brittany.Internal.Layouters.Type
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Stmt
import           Language.Haskell.Brittany.Internal.Layouters.Pattern
import           Language.Haskell.Brittany.Internal.Utils

import           Bag ( mapBagM )



layoutDataDecl
  :: Located (HsDecl RdrName)
  -> Located RdrName
  -> LHsQTyVars RdrName
  -> HsDataDefn RdrName
  -> ToBriDocM BriDocNumbered
layoutDataDecl ld name (HsQTvs _ bndrs _) defn = case defn of
  HsDataDefn NewType (L _ []) _ctype Nothing [cons] mDerivs -> case cons of
    (L _ (ConDeclH98 consName Nothing (Just (L _ [])) details _)) ->
      docWrapNode ld $ do
        nameStr     <- lrdrNameToTextAnn name
        consNameStr <- lrdrNameToTextAnn consName
        tyVarDocs   <- bndrs `forM` \case
          (L _ (UserTyVar vname)) -> return $ (lrdrNameToText vname, Nothing)
          (L _ (KindedTyVar lrdrName kind)) -> do
            d <- docSharedWrapper layoutType kind
            return $ (lrdrNameToText lrdrName, Just $ d)
        tyVarLine   <-
          fmap return
            $   docSeq
            $   List.intersperse docSeparator
            $   tyVarDocs
            <&> \(vname, mKind) -> case mKind of
                  Nothing   -> docLit vname
                  Just kind -> docSeq
                    [ docLit (Text.pack "(")
                    , docLit vname
                    , docSeparator
                    , kind
                    , docLit (Text.pack ")")
                    ]
        headDoc     <- fmap return $ docSeq
          [ appSep $ docLit (Text.pack "newtype")
          , appSep $ docLit nameStr
          , appSep tyVarLine
          ]
        rhsDoc      <- fmap return $ case details of
          PrefixCon args         -> docSeq
            [ docLit consNameStr
            , docSeparator
            , docSeq $ List.intersperse docSeparator $ args <&> layoutType
            ]
          RecCon    (L _ fields) -> docSeq
            [ appSep $ docLit $ Text.pack "{"
            , docSeq
              $   List.intersperse docSeparator
              $   fields
              <&> \(L _ (ConDeclField names t _)) -> do
                    docSeq
                      [ docSeq
                        $   List.intersperse docCommaSep
                        $   names
                        <&> \(L _ (FieldOcc fieldName _)) ->
                              docLit =<< lrdrNameToTextAnn fieldName
                      , docSeparator
                      , docLit $ Text.pack "::"
                      , docSeparator
                      , layoutType t
                      ]
            , docLit $ Text.pack "}"
            ]
          InfixCon arg1 arg2     -> docSeq
            [ layoutType arg1
            , docSeparator
            , docLit consNameStr
            , docSeparator
            , layoutType arg2
            ]
        let
          mainDoc =
            docSeq
              [ headDoc
              , docSeparator
              , docLit (Text.pack "=")
              , docSeparator
              , rhsDoc
              ]
        case mDerivs of
          Nothing                 -> mainDoc
          Just (L _ [(HsIB _ t)]) -> do
            docAddBaseY BrIndentRegular $ docPar mainDoc $ docSeq
              [docLit $ Text.pack "deriving", docSeparator, layoutType t]
          Just (L _ ts          ) -> do
            docAddBaseY BrIndentRegular $ docPar mainDoc $ docSeq
              [ docLit $ Text.pack "deriving"
              , docSeparator
              , docLit $ Text.pack "("
              , docSeq $ List.intersperse docCommaSep $ ts <&> \(HsIB _ t) ->
                layoutType t
              , docLit $ Text.pack ")"
              ]
    _ -> briDocByExactNoComment ld

  -- HsDataDefn DataType _ctxt _ctype Nothing _conss _derivs -> do
  --   -- _ name vars ctxt ctype mKindSig conss derivs
  --   nameStr <- lrdrNameToTextAnn name
  --   docLit nameStr

  _ -> briDocByExactNoComment ld

