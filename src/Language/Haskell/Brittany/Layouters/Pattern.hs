{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Layouters.Pattern
  ( layoutPat
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Types
import           Language.Haskell.Brittany.LayoutBasics

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           SrcLoc ( SrcSpan )
import           HsSyn
import           Name
import           BasicTypes

import {-# SOURCE #-} Language.Haskell.Brittany.Layouters.Expr



layoutPat :: ToBriDoc Pat
layoutPat lpat@(L _ pat) = fmap (docWrapNode lpat) $ case pat of
  WildPat _  -> return $ BDLit $ Text.pack "_"
  VarPat n   -> return $ BDLit $ lrdrNameToText n
  LitPat lit -> return $ litBriDoc lit
  ParPat inner -> do
    innerDoc <- layoutPat inner
    return $ BDSeq
      [ BDLit $ Text.pack "("
      , innerDoc
      , BDLit $ Text.pack ")"
      ]
  ConPatIn lname (PrefixCon args) -> do
    let nameDoc = lrdrNameToText lname
    argDocs <- layoutPat `mapM` args
    return $ BDSeq $
      appSep (BDLit nameDoc) : spacifyDocs argDocs
  ConPatIn lname (InfixCon left right) -> do
    let nameDoc = lrdrNameToText lname
    leftDoc  <- layoutPat left
    rightDoc <- layoutPat right
    return $ BDSeq [leftDoc, BDLit nameDoc, rightDoc]
  TuplePat args boxity _ -> do
    argDocs <- layoutPat `mapM` args
    return $ case boxity of
      Boxed -> BDAlt
        [ BDSeq
        $  [ BDLit $ Text.pack "(" ]
        ++ List.intersperse (appSep $ BDLit $ Text.pack ",") argDocs
        ++ [ BDLit $ Text.pack ")"]
        -- TODO
        ]
      Unboxed -> BDAlt
        [ BDSeq
        $  [ BDLit $ Text.pack "(#" ]
        ++ List.intersperse (appSep $ BDLit $ Text.pack ",") argDocs
        ++ [ BDLit $ Text.pack "#)"]
        -- TODO
        ]
  AsPat asName asPat -> do
    patDoc <- layoutPat asPat
    return $ BDSeq
      [ BDLit $ lrdrNameToText asName <> Text.pack "@"
      , patDoc
      ]
-- #if MIN_VERSION_ghc(8,0,0)
--   VarPat n -> return $ stringLayouter lpat $ lrdrNameToText n
-- #else
--   VarPat n -> return $ stringLayouter lpat $ rdrNameToText n
-- #endif
  _ -> briDocByExact lpat
