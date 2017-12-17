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
import GHC (unLoc, runGhc, GenLocated(L), moduleNameString, AnnKeywordId(..))
import           HsSyn
import           Name
import           HsImpExp
import           FieldLabel
import qualified FastString
import           BasicTypes

import           Language.Haskell.Brittany.Internal.Utils


layoutIE :: ToBriDoc IE
layoutIE lie@(L _ _ie) =
  docWrapNode lie
    $ let
        ien = docLit $ rdrNameToText $ ieName _ie
      in
        case _ie of
          IEVar      _ -> ien
          IEThingAbs _ -> ien
          IEThingAll _ -> docSeq [ien, docLit $ Text.pack "(..)"]
          IEThingWith _ (IEWildcard _) _ _ ->
            docSeq [ien, docLit $ Text.pack "(..)"]
          IEThingWith _ _ ns fs ->
            let
              prepareFL =
                docLit . Text.pack . FastString.unpackFS . flLabel . unLoc
            in
              docSeq
              $  [ien, docLit $ Text.pack "("]
#if MIN_VERSION_ghc(8,2,0)
              ++ (  intersperse docCommaSep (map (docLit . lrdrNameToText . ieLWrappedName) ns)
#else
              ++ (  intersperse docCommaSep (map (docLit . lrdrNameToText) ns)
#endif
                 ++ intersperse docCommaSep (map (prepareFL) fs)
                 )
              ++ [docLit $ Text.pack ")"]
          IEModuleContents n -> docSeq
            [ docLit $ Text.pack "module"
            , docSeparator
            , docLit . Text.pack . moduleNameString $ unLoc n
            ]
          _ -> docEmpty

layoutIEList :: [LIE RdrName] -> ToBriDocM BriDocNumbered
layoutIEList lies = do
  ies <- mapM (docSharedWrapper layoutIE) lies
  case ies of
    []     -> docLit $ Text.pack "()"
    (x:xs) -> docAlt
      [ docSeq
      $  [docLit $ Text.pack "(", x]
      ++ map (\x' -> docSeq [docCommaSep, x']) xs
      ++ [docLit $ Text.pack ")"]
      , docLines
        (  docSeq [docLit $ Text.pack "(", docSeparator, x]
        :  map (\x' -> docSeq [docCommaSep, x']) xs
        ++ [docLit $ Text.pack ")"]
        )
      ]
