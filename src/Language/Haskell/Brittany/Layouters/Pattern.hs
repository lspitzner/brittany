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
import           Language.Haskell.Brittany.Layouters.Type



layoutPat :: ToBriDoc Pat
layoutPat lpat@(L _ pat) = docWrapNode lpat $ case pat of
  WildPat _  -> docLit $ Text.pack "_"
  VarPat n   -> docLit $ lrdrNameToText n
  LitPat lit -> allocateNode $ litBriDoc lit
  ParPat inner -> do
    innerDoc <- docSharedWrapper layoutPat inner
    docSeq
      [ docLit $ Text.pack "("
      , innerDoc
      , docLit $ Text.pack ")"
      ]
  ConPatIn lname (PrefixCon args) -> do
    let nameDoc = lrdrNameToText lname
    argDocs <- docSharedWrapper layoutPat `mapM` args
    if null argDocs
      then docLit nameDoc
      else docSeq
             $ appSep (docLit nameDoc) : spacifyDocs argDocs
  ConPatIn lname (InfixCon left right) -> do
    let nameDoc = lrdrNameToText lname
    leftDoc  <- docSharedWrapper layoutPat left
    rightDoc <- docSharedWrapper layoutPat right
    docSeq [leftDoc, docLit nameDoc, rightDoc]
  ConPatIn lname (RecCon (HsRecFields [] Nothing)) -> do
    let t = lrdrNameToText lname
    docLit $ t <> Text.pack "{}"
  ConPatIn lname (RecCon (HsRecFields fs@(_:_) Nothing)) -> do
    let t = lrdrNameToText lname
    fds <- fs `forM` \(L _ (HsRecField (L _ (FieldOcc lnameF _)) fPat _)) -> do
      fExpDoc <- docSharedWrapper layoutPat fPat
      return $ (lrdrNameToText lnameF, fExpDoc)
    docSeq
      [ appSep $ docLit t
      , appSep $ docLit $ Text.pack "{"
      , docSeq $ List.intersperse docCommaSep
               $ fds <&> \(fieldName, fieldDoc) -> docSeq
          [ appSep $ docLit $ fieldName
          , appSep $ docLit $ Text.pack "="
          , fieldDoc
          ]
      , docLit $ Text.pack "}"
      ]
  TuplePat args boxity _ -> do
    argDocs <- docSharedWrapper layoutPat `mapM` args
    case boxity of
      Boxed -> docAlt
        [ docSeq
        $  [ docLit $ Text.pack "(" ]
        ++ List.intersperse (appSep $ docLit $ Text.pack ",") argDocs
        ++ [ docLit $ Text.pack ")"]
        -- TODO
        ]
      Unboxed -> docAlt
        [ docSeq
        $  [ docLit $ Text.pack "(#" ]
        ++ List.intersperse (appSep $ docLit $ Text.pack ",") argDocs
        ++ [ docLit $ Text.pack "#)"]
        -- TODO
        ]
  AsPat asName asPat -> do
    patDoc <- docSharedWrapper layoutPat asPat
    docSeq
      [ docLit $ lrdrNameToText asName <> Text.pack "@"
      , patDoc
      ]
  SigPatIn pat1 (HsIB _ (HsWC _ _ ty1)) -> do
    patDoc <- docSharedWrapper layoutPat pat1
    tyDoc <- docSharedWrapper layoutType ty1
    docSeq
      [ appSep $ patDoc
      , appSep $ docLit $ Text.pack "::"
      , tyDoc
      ]
  ListPat elems _ _ -> do
    elemDocs <- docSharedWrapper layoutPat `mapM` elems
    docSeq
      $  [docLit $ Text.pack "["]
      ++ List.intersperse docCommaSep (elemDocs)
      ++ [docLit $ Text.pack "]"]
  BangPat pat1 -> do
    patDoc <- docSharedWrapper layoutPat pat1
    docSeq [docLit $ Text.pack "!", patDoc]
  NPat llit@(L _ (OverLit olit _ _ _)) _ _ _ -> do
    docWrapNode llit $ allocateNode $ overLitValBriDoc olit

-- #if MIN_VERSION_ghc(8,0,0)
--   VarPat n -> return $ stringLayouter lpat $ lrdrNameToText n
-- #else
--   VarPat n -> return $ stringLayouter lpat $ rdrNameToText n
-- #endif
  _ -> briDocByExact lpat
