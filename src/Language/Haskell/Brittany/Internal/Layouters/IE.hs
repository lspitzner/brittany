module Language.Haskell.Brittany.Internal.Layouters.IE
  ( layoutIE
  , layoutLLIEs
  , layoutAnnAndSepLLIEs
  , SortItemsFlag(..)
  )
where

#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Config.Types

import           GHC                            ( unLoc
                                                , GenLocated(L)
                                                , moduleNameString
                                                , AnnKeywordId(..)
                                                , Located
                                                , runGhc
                                                , ModuleName
                                                )
import           HsSyn
import           HsImpExp
import           FieldLabel
import qualified FastString
import           BasicTypes

import           Language.Haskell.Brittany.Internal.Utils



#if MIN_VERSION_ghc(8,2,0)
prepareName :: LIEWrappedName name -> Located name
prepareName = ieLWrappedName
#else
prepareName :: Located name -> Located name
prepareName = id
#endif

layoutIE :: ToBriDoc IE
layoutIE lie@(L _ ie) = docWrapNode lie $ case ie of
#if MIN_VERSION_ghc(8,6,0)
  IEVar _ x -> layoutWrapped lie x
#else
  IEVar x -> layoutWrapped lie x
#endif
#if MIN_VERSION_ghc(8,6,0)
  IEThingAbs _ x -> layoutWrapped lie x
#else
  IEThingAbs x -> layoutWrapped lie x
#endif
#if MIN_VERSION_ghc(8,6,0)
  IEThingAll _ x -> docSeq [layoutWrapped lie x, docLit $ Text.pack "(..)"]
#else
  IEThingAll x -> docSeq [layoutWrapped lie x, docLit $ Text.pack "(..)"]
#endif
#if MIN_VERSION_ghc(8,6,0)
  IEThingWith _ x (IEWildcard _) _ _ ->
#else
  IEThingWith x (IEWildcard _) _ _ ->
#endif
    docSeq [layoutWrapped lie x, docLit $ Text.pack "(..)"]
#if MIN_VERSION_ghc(8,6,0)
  IEThingWith _ x _ ns _ -> do
#else
  IEThingWith x _ ns _ -> do
#endif
    hasComments <- orM
      ( hasCommentsBetween lie AnnOpenP AnnCloseP
      : hasAnyCommentsBelow x
      : map hasAnyCommentsBelow ns
      )
    let sortedNs = List.sortOn wrappedNameToText ns
    runFilteredAlternative $ do
      addAlternativeCond (not hasComments)
        $  docSeq
        $  [layoutWrapped lie x, docLit $ Text.pack "("]
        ++ intersperse docCommaSep (map nameDoc sortedNs)
        ++ [docParenR]
      addAlternative
        $ docWrapNodeRest lie
        $ docAddBaseY BrIndentRegular
        $ docPar
            (layoutWrapped lie x)
            (layoutItems (splitFirstLast sortedNs))
   where
    nameDoc = (docLit =<<) . lrdrNameToTextAnn . prepareName
    layoutItem n = docSeq [docCommaSep, docWrapNode n $ nameDoc n]
    layoutItems FirstLastEmpty = docSetBaseY $ docLines
      [docSeq [docParenLSep, docNodeAnnKW lie (Just AnnOpenP) docEmpty], docParenR]
    layoutItems (FirstLastSingleton n) = docSetBaseY $ docLines
      [docSeq [docParenLSep, docNodeAnnKW lie (Just AnnOpenP) $ nameDoc n], docParenR]
    layoutItems (FirstLast n1 nMs nN) =
      docSetBaseY
        $  docLines
        $  [docSeq [docParenLSep, docWrapNode n1 $ nameDoc n1]]
        ++ map layoutItem nMs
        ++ [docSeq [docCommaSep, docNodeAnnKW lie (Just AnnOpenP) $ nameDoc nN], docParenR]
#if MIN_VERSION_ghc(8,6,0)
  IEModuleContents _ n -> docSeq
#else
  IEModuleContents n -> docSeq
#endif
    [ docLit $ Text.pack "module"
    , docSeparator
    , docLit . Text.pack . moduleNameString $ unLoc n
    ]
  _ -> docEmpty
 where
#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2, 8.4, .. */
  layoutWrapped _ = \case
    L _ (IEName    n) -> docLit =<< lrdrNameToTextAnn n
    L _ (IEPattern n) -> do
      name <- lrdrNameToTextAnn n
      docLit $ Text.pack "pattern " <> name
    L _ (IEType n) -> do
      name <- lrdrNameToTextAnn n
      docLit $ Text.pack "type " <> name
#else                      /* ghc-8.0 */
  layoutWrapped outer n = do
    name <- lrdrNameToTextAnn n
    hasType <- hasAnnKeyword n AnnType
    hasPattern <- hasAnnKeyword outer AnnPattern
    docLit $ if
      | hasType    -> Text.pack "type (" <> name <> Text.pack ")"
      | hasPattern -> Text.pack "pattern " <> name
      | otherwise  -> name
#endif

data SortItemsFlag = ShouldSortItems | KeepItemsUnsorted
-- Helper function to deal with Located lists of LIEs.
-- In particular this will also associate documentation
-- from the located list that actually belongs to the last IE.
-- It also adds docCommaSep to all but the first element
-- This configuration allows both vertical and horizontal
-- handling of the resulting list. Adding parens is
-- left to the caller since that is context sensitive
layoutAnnAndSepLLIEs
  :: SortItemsFlag -> Located [LIE GhcPs] -> ToBriDocM [ToBriDocM BriDocNumbered]
layoutAnnAndSepLLIEs shouldSort llies@(L _ lies) = do
  let makeIENode ie = docSeq [docCommaSep, ie]
  let sortedLies =
        [ items
        | group <- Data.List.Extra.groupOn lieToText
          $ List.sortOn lieToText lies
        , items <- mergeGroup group
        ]
  let ieDocs = fmap layoutIE $ case shouldSort of
        ShouldSortItems -> sortedLies
        KeepItemsUnsorted -> lies
  ieCommaDocs <-
    docWrapNodeRest llies $ sequence $ case splitFirstLast ieDocs of
      FirstLastEmpty        -> []
      FirstLastSingleton ie -> [ie]
      FirstLast ie1 ieMs ieN ->
        [ie1] ++ map makeIENode ieMs ++ [makeIENode ieN]
  pure $ fmap pure ieCommaDocs -- returned shared nodes
 where
  mergeGroup :: [LIE GhcPs] -> [LIE GhcPs]
  mergeGroup []        = []
  mergeGroup items@[_] = items
  mergeGroup items     = if
    | all isProperIEThing items -> [List.foldl1' thingFolder items]
    | all isIEVar items         -> [List.foldl1' thingFolder items]
    | otherwise                 -> items
  -- proper means that if it is a ThingWith, it does not contain a wildcard
  -- (because I don't know what a wildcard means if it is not already a
  -- IEThingAll).
  isProperIEThing :: LIE GhcPs -> Bool
#if MIN_VERSION_ghc(8,6,0) /* 8.6+ */
  isProperIEThing = \case
    L _ (IEThingAbs _ _wn) -> True
    L _ (IEThingAll _ _wn) -> True
    L _ (IEThingWith _ _wn NoIEWildcard _ _) -> True
    _ -> False
#else                      /* 8.0 8.2 8.4 */
  isProperIEThing = \case
    L _ (IEThingAbs _wn) -> True
    L _ (IEThingAll _wn) -> True
    L _ (IEThingWith _wn NoIEWildcard _ _) -> True
    _ -> False
#endif
  isIEVar :: LIE GhcPs -> Bool
  isIEVar = \case
    L _ IEVar{} -> True
    _           -> False
  thingFolder :: LIE GhcPs -> LIE GhcPs -> LIE GhcPs
  thingFolder l1@(L _ IEVar{}     ) _                     = l1
  thingFolder l1@(L _ IEThingAll{}) _                     = l1
  thingFolder _                     l2@(L _ IEThingAll{}) = l2
  thingFolder l1                    (   L _ IEThingAbs{}) = l1
  thingFolder (L _ IEThingAbs{})    l2                    = l2
#if MIN_VERSION_ghc(8,6,0) /* 8.6+ */
  thingFolder (L l (IEThingWith x wn _ consItems1 fieldLbls1)) (L _ (IEThingWith _ _ _ consItems2 fieldLbls2))
    = L
      l
      (IEThingWith x
                   wn
                   NoIEWildcard
                   (consItems1 ++ consItems2)
                   (fieldLbls1 ++ fieldLbls2)
      )
#else                      /* 8.0 8.2 8.4 */
  thingFolder (L l (IEThingWith wn _ consItems1 fieldLbls1)) (L _ (IEThingWith _ _ consItems2 fieldLbls2))
    = L
      l
      (IEThingWith wn
                   NoIEWildcard
                   (consItems1 ++ consItems2)
                   (fieldLbls1 ++ fieldLbls2)
      )
#endif
  thingFolder _ _ =
    error "thingFolder should be exhaustive because we have a guard above"


-- Builds a complete layout for the given located
-- list of LIEs. The layout provides two alternatives:
-- (item, item, ..., item)
-- ( item
-- , item
-- ...
-- , item
-- )
-- If the llies contains comments the list will
-- always expand over multiple lines, even when empty:
-- () -- no comments
-- ( -- a comment
-- )
layoutLLIEs :: Bool -> SortItemsFlag -> Located [LIE GhcPs] -> ToBriDocM BriDocNumbered
layoutLLIEs enableSingleline shouldSort llies = do
  ieDs        <- layoutAnnAndSepLLIEs shouldSort llies
  hasComments <- hasAnyCommentsBelow llies
  runFilteredAlternative $ case ieDs of
    [] -> do
      addAlternativeCond (not hasComments) $ docLit $ Text.pack "()"
      addAlternativeCond hasComments $ docPar
        (docSeq [docParenLSep, docWrapNodeRest llies docEmpty])
        docParenR
    (ieDsH : ieDsT) -> do
      addAlternativeCond (not hasComments && enableSingleline)
        $  docSeq
        $  [docLit (Text.pack "(")]
        ++ (docForceSingleline <$> ieDs)
        ++ [docParenR]
      addAlternative
        $  docPar (docSetBaseY $ docSeq [docParenLSep, ieDsH])
        $  docLines
        $  ieDsT
        ++ [docParenR]

-- | Returns a "fingerprint string", not a full text representation, nor even
-- a source code representation of this syntax node.
-- Used for sorting, not for printing the formatter's output source code.
#if MIN_VERSION_ghc(8,2,0)
wrappedNameToText :: LIEWrappedName RdrName -> Text
wrappedNameToText = \case
  L _ (IEName    n) -> lrdrNameToText n
  L _ (IEPattern n) -> lrdrNameToText n
  L _ (IEType    n) -> lrdrNameToText n
#else
wrappedNameToText :: Located RdrName -> Text
wrappedNameToText = lrdrNameToText
#endif
-- | Returns a "fingerprint string", not a full text representation, nor even
-- a source code representation of this syntax node.
-- Used for sorting, not for printing the formatter's output source code.
lieToText :: LIE GhcPs -> Text
#if MIN_VERSION_ghc(8,6,0) /* 8.6+ */
lieToText = \case
  L _ (IEVar      _ wn       ) -> wrappedNameToText wn
  L _ (IEThingAbs _ wn       ) -> wrappedNameToText wn
  L _ (IEThingAll _ wn       ) -> wrappedNameToText wn
  L _ (IEThingWith _ wn _ _ _) -> wrappedNameToText wn
  -- TODO: These _may_ appear in exports!
  -- Need to check, and either put them at the top (for module) or do some
  -- other clever thing.
  L _ (IEModuleContents _ n) -> moduleNameToText n
  L _ (IEGroup _ _ _         ) -> Text.pack "@IEGroup"
  L _ (IEDoc      _ _        ) -> Text.pack "@IEDoc"
  L _ (IEDocNamed _ _        ) -> Text.pack "@IEDocNamed"
  L _ (XIE _                 ) -> Text.pack "@XIE"
#else                      /* 8.0 8.2 8.4 */                    
lieToText = \case
  L _ (IEVar      wn       ) -> wrappedNameToText wn
  L _ (IEThingAbs wn       ) -> wrappedNameToText wn
  L _ (IEThingAll wn       ) -> wrappedNameToText wn
  L _ (IEThingWith wn _ _ _) -> wrappedNameToText wn
  -- TODO: These _may_ appear in exports!
  -- Need to check, and either put them at the top (for module) or do some
  -- other clever thing.
  L _ (IEModuleContents n  ) -> moduleNameToText n
  L _ (IEGroup _ _         ) -> Text.pack "@IEGroup"
  L _ (IEDoc      _        ) -> Text.pack "@IEDoc"
  L _ (IEDocNamed _        ) -> Text.pack "@IEDocNamed"
#endif
 where
  moduleNameToText :: Located ModuleName -> Text
  moduleNameToText (L _ name) = Text.pack ("@IEModuleContents" ++ moduleNameString name)
