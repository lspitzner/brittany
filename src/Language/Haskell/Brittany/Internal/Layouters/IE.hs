module Language.Haskell.Brittany.Internal.Layouters.IE
  ( layoutIE
  , layoutIEList
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
                        (map (docLit . lrdrNameToText . prepareName) ns)
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

layoutIEList :: [LIE RdrName] -> ToBriDocM BriDocNumbered
layoutIEList lies = do
  ies <- mapM (docSharedWrapper layoutIE) lies
  case ies of
    []         -> docLit $ Text.pack "()"
    xs@(x1:xr) -> docAlt
      [ docSeq
        [ docLit $ Text.pack "("
        , docSeq $ List.intersperse docCommaSep xs
        , docLit $ Text.pack ")"
        ]
      , docLines
        (  [docSeq [docParenLSep, x1]]
        ++ [ docSeq [docCommaSep, x] | x <- xr ]
        ++ [docParenR]
        )
      ]
