module Language.Haskell.Brittany.Internal.Layouters.IE
  ( layoutIE
  , layoutLLIEs
  , layoutAnnAndSepLLIEs
  )
where

#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Config.Types

import           GHC     ( unLoc
                         , runGhc
                         , GenLocated(L)
                         , moduleNameString
                         , AnnKeywordId(..)
                         , Located
                         )
#if MIN_VERSION_ghc(8,10,1)   /* ghc-8.10.1 */
import           GHC.Hs
import           GHC.Hs.ImpExp
#else
import           HsSyn
import           HsImpExp
#endif
import           Name
import           FieldLabel
import qualified FastString
import           BasicTypes

import           Language.Haskell.Brittany.Internal.Utils



prepareName :: LIEWrappedName name -> Located name
prepareName = ieLWrappedName

layoutIE :: ToBriDoc IE
layoutIE lie@(L _ ie) = docWrapNode lie $ case ie of
  IEVar _ x -> layoutWrapped lie x
  IEThingAbs _ x -> layoutWrapped lie x
  IEThingAll _ x -> docSeq [layoutWrapped lie x, docLit $ Text.pack "(..)"]
  IEThingWith _ x (IEWildcard _) _ _ ->
    docSeq [layoutWrapped lie x, docLit $ Text.pack "(..)"]
  IEThingWith _ x _ ns _ -> do
    hasComments <- orM
      ( hasCommentsBetween lie AnnOpenP AnnCloseP
      : hasAnyCommentsBelow x
      : map hasAnyCommentsBelow ns
      )
    runFilteredAlternative $ do
      addAlternativeCond (not hasComments)
        $  docSeq
        $  [layoutWrapped lie x, docLit $ Text.pack "("]
        ++ intersperse docCommaSep (map nameDoc ns)
        ++ [docParenR]
      addAlternative
        $ docWrapNodeRest lie
        $ docAddBaseY BrIndentRegular
        $ docPar
            (layoutWrapped lie x)
            (layoutItems (splitFirstLast ns))
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
  IEModuleContents _ n -> docSeq
    [ docLit $ Text.pack "module"
    , docSeparator
    , docLit . Text.pack . moduleNameString $ unLoc n
    ]
  _ -> docEmpty
 where
  layoutWrapped _ = \case
    L _ (IEName    n) -> docLit =<< lrdrNameToTextAnn n
    L _ (IEPattern n) -> do
      name <- lrdrNameToTextAnn n
      docLit $ Text.pack "pattern " <> name
    L _ (IEType n) -> do
      name <- lrdrNameToTextAnn n
      docLit $ Text.pack "type " <> name

-- Helper function to deal with Located lists of LIEs.
-- In particular this will also associate documentation
-- from the located list that actually belongs to the last IE.
-- It also adds docCommaSep to all but the first element
-- This configuration allows both vertical and horizontal
-- handling of the resulting list. Adding parens is
-- left to the caller since that is context sensitive
layoutAnnAndSepLLIEs
  :: Located [LIE GhcPs] -> ToBriDocM [ToBriDocM BriDocNumbered]
layoutAnnAndSepLLIEs llies@(L _ lies) = do
  let makeIENode ie = docSeq [docCommaSep, ie]
  let ieDocs = layoutIE <$> lies
  ieCommaDocs <-
    docWrapNodeRest llies $ sequence $ case splitFirstLast ieDocs of
      FirstLastEmpty        -> []
      FirstLastSingleton ie -> [ie]
      FirstLast ie1 ieMs ieN ->
        [ie1] ++ map makeIENode ieMs ++ [makeIENode ieN]
  pure $ fmap pure ieCommaDocs -- returned shared nodes

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
layoutLLIEs :: Bool -> Located [LIE GhcPs] -> ToBriDocM BriDocNumbered
layoutLLIEs enableSingleline llies = do
  ieDs        <- layoutAnnAndSepLLIEs llies
  hasComments <- hasAnyCommentsBelow llies
  runFilteredAlternative $
    case ieDs of
      [] -> do
        addAlternativeCond (not hasComments) $
          docLit $ Text.pack "()"
        addAlternativeCond hasComments $
          docPar (docSeq [docParenLSep, docWrapNodeRest llies docEmpty])
                 docParenR
      (ieDsH:ieDsT) -> do
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
