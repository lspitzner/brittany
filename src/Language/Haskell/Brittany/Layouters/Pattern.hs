{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Layouters.Pattern
  ( layoutPat
  , colsWrapPat
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Types
import           Language.Haskell.Brittany.LayouterBasics

import           RdrName ( RdrName(..) )
import           GHC ( Located, runGhc, GenLocated(L), moduleNameString )
import           HsSyn
import           Name
import           BasicTypes

import {-# SOURCE #-} Language.Haskell.Brittany.Layouters.Expr
import           Language.Haskell.Brittany.Layouters.Type



layoutPat :: ToBriDocC (Pat RdrName) (Seq BriDocNumbered)
layoutPat lpat@(L _ pat) = docWrapNode lpat $ case pat of
  WildPat _  -> fmap Seq.singleton $ docLit $ Text.pack "_"
  VarPat n   -> fmap Seq.singleton $ docLit $ lrdrNameToText n
  LitPat lit -> fmap Seq.singleton $ allocateNode $ litBriDoc lit
  ParPat inner -> do
    left  <- docLit $ Text.pack "("
    right <- docLit $ Text.pack ")"
    innerDocs <- colsWrapPat =<< layoutPat inner
    return $ Seq.empty Seq.|> left Seq.|> innerDocs Seq.|> right
    -- return $ (left Seq.<| innerDocs) Seq.|> right
    -- case Seq.viewl innerDocs of
    --   Seq.EmptyL -> fmap return $ docLit $ Text.pack "()" -- this should never occur..
    --   x1 Seq.:< rest -> case Seq.viewr rest of
    --     Seq.EmptyR ->
    --       fmap return $ docSeq
    --       [ docLit $ Text.pack "("
    --       , return x1
    --       , docLit $ Text.pack ")"
    --       ]
    --     middle Seq.:> xN -> do
    --       x1' <- docSeq [docLit $ Text.pack "(", return x1]
    --       xN' <- docSeq [return xN, docLit $ Text.pack ")"]
    --       return $ (x1' Seq.<| middle) Seq.|> xN'
  ConPatIn lname (PrefixCon args) -> do
    let nameDoc = lrdrNameToText lname
    argDocs <- layoutPat `mapM` args
    if null argDocs
      then return <$> docLit nameDoc
      else do
        x1 <- appSep (docLit nameDoc)
        xR <- fmap Seq.fromList
          $ sequence
          $ spacifyDocs
          $ fmap colsWrapPat argDocs
        return $ x1 Seq.<| xR
  ConPatIn lname (InfixCon left right) -> do
    let nameDoc = lrdrNameToText lname
    leftDoc  <- colsWrapPat =<< layoutPat left
    rightDoc <- colsWrapPat =<< layoutPat right
    middle <- docLit nameDoc
    return $ Seq.empty Seq.|> leftDoc Seq.|> middle Seq.|> rightDoc
  ConPatIn lname (RecCon (HsRecFields [] Nothing)) -> do
    let t = lrdrNameToText lname
    fmap Seq.singleton $ docLit $ t <> Text.pack "{}"
  ConPatIn lname (RecCon (HsRecFields fs@(_:_) Nothing)) -> do
    let t = lrdrNameToText lname
    fds <- fs `forM` \(L _ (HsRecField (L _ (FieldOcc lnameF _)) fPat pun)) -> do
      fExpDoc <- if pun
        then return Nothing
        else Just <$> docSharedWrapper layoutPat fPat
      return $ (lrdrNameToText lnameF, fExpDoc)
    fmap Seq.singleton $ docSeq
      [ appSep $ docLit t
      , appSep $ docLit $ Text.pack "{"
      , docSeq $ List.intersperse docCommaSep
               $ fds <&> \case
          (fieldName, Just fieldDoc) -> docSeq
            [ appSep $ docLit $ fieldName
            , appSep $ docLit $ Text.pack "="
            , fieldDoc >>= colsWrapPat
            ]
          (fieldName, Nothing) -> docLit fieldName
      , docSeparator
      , docLit $ Text.pack "}"
      ]
  ConPatIn lname (RecCon (HsRecFields [] (Just 0))) -> do
    let t = lrdrNameToText lname
    fmap Seq.singleton $ docSeq
      [ appSep $ docLit t
      , docLit $ Text.pack "{..}"
      ]
  TuplePat args boxity _ -> do
    case boxity of
      Boxed   -> wrapPatListy args "(" ")"
      Unboxed -> wrapPatListy args "(#" "#)"
  AsPat asName asPat -> do
    wrapPatPrepend asPat (docLit $ lrdrNameToText asName <> Text.pack "@")
  SigPatIn pat1 (HsIB _ (HsWC _ _ ty1)) -> do
    patDocs <- layoutPat pat1
    tyDoc <- docSharedWrapper layoutType ty1
    case Seq.viewr patDocs of
      Seq.EmptyR -> error "cannot happen ljoiuxoasdcoviuasd"
      xR Seq.:> xN -> do
        xN' <- -- at the moment, we don't support splitting patterns into
               -- multiple lines. but we cannot enforce pasting everything
               -- into one line either, because the type signature will ignore
               -- this if we overflow sufficiently.
               -- In order to prevent syntactically invalid results in such
               -- cases, we need the AddBaseY here.
               -- This can all change when patterns get multiline support.
               docAddBaseY BrIndentRegular $ docSeq
          [ appSep $ return xN
          , appSep $ docLit $ Text.pack "::"
          , docForceSingleline $ tyDoc
          ]
        return $ xR Seq.|> xN'
  ListPat elems _ _ ->
    wrapPatListy elems "[" "]"
  BangPat pat1 -> do
    wrapPatPrepend pat1 (docLit $ Text.pack "!")
  LazyPat pat1 -> do
    wrapPatPrepend pat1 (docLit $ Text.pack "~")
  NPat llit@(L _ (OverLit olit _ _ _)) _ _ _ -> do
    fmap Seq.singleton $ docWrapNode llit $ allocateNode $ overLitValBriDoc olit

-- #if MIN_VERSION_ghc(8,0,0)
--   VarPat n -> return $ stringLayouter lpat $ lrdrNameToText n
-- #else
--   VarPat n -> return $ stringLayouter lpat $ rdrNameToText n
-- #endif
  _ -> fmap return $ briDocByExactInlineOnly "some unknown pattern" lpat

colsWrapPat :: Seq BriDocNumbered -> ToBriDocM BriDocNumbered
colsWrapPat = docCols ColPatterns . fmap return . Foldable.toList

wrapPatPrepend
  :: Located (Pat RdrName)
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM (Seq BriDocNumbered)
wrapPatPrepend pat prepElem = do
  patDocs <- layoutPat pat
  case Seq.viewl patDocs of
    Seq.EmptyL -> return $ Seq.empty
    x1 Seq.:< xR -> do
      x1' <- docSeq [prepElem, return x1]
      return $ x1' Seq.<| xR

wrapPatListy
  :: [Located (Pat RdrName)]
  -> String
  -> String
  -> ToBriDocM (Seq BriDocNumbered)
wrapPatListy elems start end = do
  elemDocs <- Seq.fromList elems `forM` \e -> layoutPat e >>= colsWrapPat
  sDoc <- docLit $ Text.pack start
  eDoc <- docLit $ Text.pack end
  case Seq.viewl elemDocs of
    Seq.EmptyL -> fmap Seq.singleton $ docLit $ Text.pack $ start ++ end
    x1 Seq.:< rest -> do
        rest' <- rest `forM` \bd -> docSeq
          [ docLit $ Text.pack ","
          , docSeparator
          , return bd
          ]
        return $ (sDoc Seq.<| x1 Seq.<| rest') Seq.|> eDoc
