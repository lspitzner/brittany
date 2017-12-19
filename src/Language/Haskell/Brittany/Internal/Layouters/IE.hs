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
      ++ (  intersperse docCommaSep
                        (map ((docLit =<<) . lrdrNameToTextAnn . prepareName) ns)
         ++ intersperse docCommaSep (map prepareFL fs)
         )
      ++ [docLit $ Text.pack ")"]
   where
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
-- from the LIES that actually belongs to the last IE.
-- It also add docCommaSep to all but he last element
-- This configuration allows both vertical and horizontal
-- handling of the resulting list. Adding parens is
-- left to the caller since that is context sensitive
layoutAnnAndSepLLIEs :: (Located [LIE RdrName]) -> ToBriDocM [ToBriDocM BriDocNumbered]
layoutAnnAndSepLLIEs llies@(L _ lies) = do
  let makeIENode ie = docSeq [docCommaSep, ie]
      layoutAnnAndSepLLIEs' ies = case ies of
          [] -> []
          [ie] -> [docWrapNode llies $ ie]
          (ie:ies') -> ie:map makeIENode (List.init ies')
                 ++ [makeIENode $ docWrapNode llies $ List.last ies']
  layoutAnnAndSepLLIEs' <$> mapM (docSharedWrapper layoutIE) lies

-- Builds a complete layout for the given located
-- list of LIEs. The layout provides two alternatives:
-- (item, item, ..., item)
-- ( item
-- , item
-- ...
-- , item
-- )
-- Empty lists will always be rendered as ()
layoutLLIEs :: Located [LIE RdrName] -> ToBriDocM BriDocNumbered
layoutLLIEs llies = docWrapNodeRest llies $ do
  ieDs <- layoutAnnAndSepLLIEs llies
  case ieDs of
    [] -> docLit $ Text.pack "()"
    ieDs@(ieDsH:ieDsT) ->
      docAlt
        [ docSeq $ docLit (Text.pack "("):ieDs ++ [docParenR]
        , docLines $
            docSeq [docParenLSep, ieDsH]
            : ieDsT
            ++ [docParenR]
        ]
