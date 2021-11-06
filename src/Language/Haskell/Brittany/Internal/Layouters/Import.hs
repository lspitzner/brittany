module Language.Haskell.Brittany.Internal.Layouters.Import (layoutImport) where

import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import qualified Control.Monad.Reader.Class as Reader.Class
import qualified Control.Monad.RWS.Class as RWS.Class
import qualified Control.Monad.State.Class as State.Class
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.MultiRWS.Lazy as MultiRWSL
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.State.Lazy as StateL
import qualified Control.Monad.Trans.State.Strict as StateS
import qualified Control.Monad.Writer.Class as Writer.Class
import qualified Data.Bool as Bool
import qualified Data.ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Coerce
import qualified Data.Data
import qualified Data.Either
import qualified Data.Foldable
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Lazy as IntMapL
import qualified Data.IntMap.Strict as IntMapS
import qualified Data.List.Extra
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL.Encoding
import qualified Data.Text.Lazy.IO as TextL.IO
import qualified GHC.OldList as List
import qualified Safe as Safe
import qualified System.Directory
import qualified System.IO
import qualified Text.PrettyPrint
import qualified Text.PrettyPrint.Annotated
import qualified Text.PrettyPrint.Annotated.HughesPJ
import qualified Text.PrettyPrint.Annotated.HughesPJClass

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
import           Language.Haskell.Brittany.Internal.Config.Types

import           GHC                                      ( unLoc
                                                          , GenLocated(L)
                                                          , moduleNameString
                                                          , Located
                                                          )
import           GHC.Hs
import           GHC.Types.Name
import           GHC.Types.FieldLabel
import qualified GHC.Data.FastString
import           GHC.Types.Basic
import GHC.Unit.Types (IsBootInterface(..))
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types

import           Language.Haskell.Brittany.Internal.Utils



prepPkg :: SourceText -> String
prepPkg rawN = case rawN of
  SourceText n -> n
  -- This would be odd to encounter and the
  -- result will most certainly be wrong
  NoSourceText -> ""
prepModName :: Located e -> e
prepModName = unLoc

layoutImport :: ImportDecl GhcPs -> ToBriDocM BriDocNumbered
layoutImport importD = case importD of
  ImportDecl _ _ (L _ modName) pkg src safe q False mas mllies -> do
    importCol <- mAsk <&> _conf_layout .> _lconfig_importColumn .> confUnpack
    importAsCol <- mAsk <&> _conf_layout .> _lconfig_importAsColumn .> confUnpack
    indentPolicy <- mAsk <&>  _conf_layout .> _lconfig_indentPolicy .> confUnpack
    let
      compact  = indentPolicy /= IndentPolicyFree
      modNameT = Text.pack $ moduleNameString modName
      pkgNameT = Text.pack . prepPkg . sl_st <$> pkg
      masT     = Text.pack . moduleNameString . prepModName <$> mas
      hiding   = maybe False fst mllies
      minQLength = length "import qualified "
      qLengthReal =
        let qualifiedPart = if q /= NotQualified then length "qualified " else 0
            safePart      = if safe then length "safe " else 0
            pkgPart       = maybe 0 ((+ 1) . Text.length) pkgNameT
            srcPart = case src of { IsBoot -> length "{-# SOURCE #-} "; NotBoot -> 0 }
        in  length "import " + srcPart + safePart + qualifiedPart + pkgPart
      qLength          = max minQLength qLengthReal
      -- Cost in columns of importColumn
      asCost           = length "as "
      hidingParenCost  = if hiding then length "hiding ( " else length "( "
      nameCost         = Text.length modNameT + qLength
      importQualifiers = docSeq
        [ appSep $ docLit $ Text.pack "import"
        , case src of { IsBoot -> appSep $ docLit $ Text.pack "{-# SOURCE #-}"; NotBoot -> docEmpty }
        , if safe then appSep $ docLit $ Text.pack "safe" else docEmpty
        , if q /= NotQualified then appSep $ docLit $ Text.pack "qualified" else docEmpty
        , maybe docEmpty (appSep . docLit) pkgNameT
        ]
      indentName =
        if compact then id else docEnsureIndent (BrIndentSpecial qLength)
      modNameD =
        indentName $ appSep $ docLit modNameT
      hidDocCol = if hiding then importCol - hidingParenCost else importCol - 2
      hidDocColDiff = importCol - 2 - hidDocCol
      hidDoc = if hiding
        then appSep $ docLit $ Text.pack "hiding"
        else docEmpty
      importHead = docSeq [importQualifiers, modNameD]
      bindingsD  = case mllies of
        Nothing -> docEmpty
        Just (_, llies) -> do
          hasComments <- hasAnyCommentsBelow llies
          if compact
          then docAlt
            [ docSeq [hidDoc, docForceSingleline $ layoutLLIEs True ShouldSortItems llies]
            , let makeParIfHiding = if hiding
                    then docAddBaseY BrIndentRegular . docPar hidDoc
                    else id
              in makeParIfHiding (layoutLLIEs True ShouldSortItems llies)
            ]
          else do
            ieDs <- layoutAnnAndSepLLIEs ShouldSortItems llies
            docWrapNodeRest llies
              $ docEnsureIndent (BrIndentSpecial hidDocCol)
              $ case ieDs of
                -- ..[hiding].( )
                [] -> if hasComments
                  then docPar
                    (docSeq [hidDoc, docParenLSep, docWrapNode llies docEmpty])
                    (docEnsureIndent (BrIndentSpecial hidDocColDiff) docParenR)
                  else docSeq [hidDoc, docParenLSep, docSeparator, docParenR]
                -- ..[hiding].( b )
                [ieD] -> runFilteredAlternative $ do
                  addAlternativeCond (not hasComments)
                    $ docSeq
                    [ hidDoc
                    , docParenLSep
                    , docForceSingleline ieD
                    , docSeparator
                    , docParenR
                    ]
                  addAlternative $ docPar
                    (docSeq [hidDoc, docParenLSep, docNonBottomSpacing ieD])
                    (docEnsureIndent (BrIndentSpecial hidDocColDiff) docParenR)
                -- ..[hiding].( b
                --            , b'
                --            )
                (ieD:ieDs') ->
                  docPar
                    (docSeq [hidDoc, docSetBaseY $ docSeq [docParenLSep, ieD]])
                    (  docEnsureIndent (BrIndentSpecial hidDocColDiff)
                    $  docLines
                    $  ieDs'
                    ++ [docParenR]
                    )
      makeAsDoc asT =
        docSeq [appSep $ docLit $ Text.pack "as", appSep $ docLit asT]
    if compact
    then
      let asDoc = maybe docEmpty makeAsDoc masT
      in docAlt
        [ docForceSingleline $ docSeq [importHead, asDoc, bindingsD]
        , docAddBaseY BrIndentRegular $
            docPar (docSeq [importHead, asDoc]) bindingsD
        ]
    else
      case masT of
        Just n -> if enoughRoom
          then docLines
                 [ docSeq [importHead, asDoc], bindingsD]
          else docLines [importHead, asDoc, bindingsD]
         where
          enoughRoom = nameCost < importAsCol - asCost
          asDoc =
            docEnsureIndent (BrIndentSpecial (importAsCol - asCost))
              $ makeAsDoc n
        Nothing -> if enoughRoom
          then docSeq [importHead, bindingsD]
          else docLines [importHead, bindingsD]
          where enoughRoom = nameCost < importCol - hidingParenCost
  _ -> docEmpty
