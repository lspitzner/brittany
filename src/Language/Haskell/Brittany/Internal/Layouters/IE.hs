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

import           RdrName (RdrName(..))
import           GHC     ( unLoc
                         , runGhc
                         , GenLocated(L)
                         , moduleNameString
                         , AnnKeywordId(..)
                         , Located
                         )
import           HsSyn
import           Name
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
  IEVar      _                     -> ien
  IEThingAbs _                     -> ien
  IEThingAll _                     -> docSeq [ien, docLit $ Text.pack "(..)"]
  IEThingWith _ (IEWildcard _) _ _ -> docSeq [ien, docLit $ Text.pack "(..)"]
  IEThingWith _ _ ns fs ->
    docSeq
      $  [ien, docLit $ Text.pack "("]
      ++ intersperse docCommaSep (map nameDoc ns ++ map prepareFL fs)
      ++ [docLit $ Text.pack ")"]
   where
    nameDoc = (docLit =<<) . lrdrNameToTextAnn . prepareName
    prepareFL = docLit . Text.pack . FastString.unpackFS . flLabel . unLoc
  IEModuleContents n -> docSeq
    [ docLit $ Text.pack "module"
    , docSeparator
    , docLit . Text.pack . moduleNameString $ unLoc n
    ]
  _ -> docEmpty
  where ien = docLit =<< lrdrNameToTextAnn (ieName <$> lie)

-- Helper function to deal with Located lists of LIEs.
-- In particular this will also associate documentation
-- from the located list that actually belongs to the last IE.
-- It also adds docCommaSep to all but the first element
-- This configuration allows both vertical and horizontal
-- handling of the resulting list. Adding parens is
-- left to the caller since that is context sensitive
layoutAnnAndSepLLIEs
  :: Located [LIE RdrName] -> ToBriDocM [ToBriDocM BriDocNumbered]
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
layoutLLIEs :: Located [LIE RdrName] -> ToBriDocM BriDocNumbered
layoutLLIEs llies = do
  ieDs <- layoutAnnAndSepLLIEs llies
  hasComments <- hasAnyCommentsBelow llies
  case ieDs of
    [] -> docAltFilter
            [ (not hasComments, docLit $ Text.pack "()")
            , (otherwise, docPar (docSeq [docParenLSep, docWrapNode llies docEmpty])
                       $ docLines [docParenR])
            ]
    (ieDsH:ieDsT) ->
      docAltFilter
        [ (not hasComments, docSeq $ docLit (Text.pack "("):ieDs ++ [docParenR])
        , (otherwise, docPar (docSetBaseY $ docSeq [docParenLSep, ieDsH]) $
            docLines $ ieDsT
            ++ [docParenR])
        ]
