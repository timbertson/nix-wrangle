{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Wrangle.Splice where

import Prelude hiding (error)
import Wrangle.Util
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty((:|)))
import System.IO.Unsafe
import Nix.Expr hiding (stripAnnotation)
import qualified Data.HashMap.Strict as HMap
import qualified Wrangle.Source as Source
import qualified Nix.Expr as Expr
import qualified Nix.Expr.Shorthands as Shorthands
import qualified Nix.Pretty as Pretty
import qualified Text.Megaparsec as Megaparsec
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc
import Data.Fix
import Nix.Parser (Result(..), parseNixTextLoc)

data Opts = Opts {
  input :: FilePath,
  output :: Maybe FilePath,
  depName :: String
}

data Indent = Indent {
  tabs :: Int,
  spaces :: Int
}

load :: FilePath -> IO T.Text
load = T.readFile

parse :: T.Text -> Result NExprLoc
parse = parseNixTextLoc

getExn :: Result NExprLoc -> IO NExprLoc
getExn (Success x) = return x
getExn (Failure f) = abort $ show f

repr :: NExprLoc -> String
repr expr = show $ stripAnnotation expr

stripAnnotation :: NExprLoc -> NExpr
stripAnnotation = Expr.stripAnnotation
pretty = Pretty.prettyNix

nixOfSrc :: Source.PackageSpec -> (NExpr, NExpr)
nixOfSrc src = (Fix . NSym . T.pack $ Source.nixName fetcher, nixAttrs fetchAttrs)
  where
    fetcher = Source.fetcherName $ Source.sourceSpec src
    fetchAttrs = HMap.toList $ Source.fetchAttrs src
    var :: String -> NAttrPath NExpr
    var s = (StaticKey (T.pack s)) :| []
    nixAttrs :: [(String, String)] -> NExpr
    nixAttrs items = Fix $ NSet $ map strBinding items
    strBinding :: (String, String) -> Binding NExpr
    strBinding (key, val) = NamedVar (var key) (Shorthands.mkStr (T.pack val)) nullPos

replaceSourceLoc :: T.Text -> Source.PackageSpec -> (Maybe (Fix NExprF), SrcSpan) -> T.Text
replaceSourceLoc orig src (originalFetcherFn, span) =
  T.unlines $
    (take (startLine-1) origLines)
      ++ [
        partialStart <>
        -- "<<<" <> (T.pack $ show $ spanBegin span) <>
        srcText <>
        -- (T.pack $ show $ spanEnd span) <> ">>>" <>
        partialEnd
      ] ++ (drop (endLine) origLines)
  where
    origLines = T.lines orig
    megaparsecTabWidth = unPos Megaparsec.defaultTabWidth
    nixIndentWidth = 2 -- hardcoded in hnix

    colWidth tabWidth '\t' = tabWidth
    colWidth _ _ = 1
    isTab = (== '\t')
    isSpace = (== ' ')

    -- TODO surely megaparsec must have some niceness for this...
    columnToIndex :: Int -> T.Text -> Int
    columnToIndex col text = columnToIndex' 0 0 (T.unpack text)
      where
        columnToIndex' _pos index [] = index
        columnToIndex' pos index (char: remainder) =
          if pos >= col
          then index
          else columnToIndex' (pos + (colWidth megaparsecTabWidth) char) (index+1) remainder

    partialStart = T.take (columnToIndex (startCol-1) line) line where line = origLines !! (startLine - 1)
    partialEnd = T.drop (columnToIndex (endCol-1) line) line where line = origLines !! (endLine - 1)
    startLine = (unPos . sourceLine . spanBegin) span
    startCol = (unPos . sourceColumn . spanBegin) span
    endLine = (unPos . sourceLine . spanEnd) span
    endCol = (unPos . sourceColumn . spanEnd) span

    (defaultFetcherFn, fetcherArgs) = nixOfSrc src
    -- TODO: warn if `originalFetcherFn` name differs meaningfully from `defaultFetcherFn`
    -- (e.g. pkgs.fetchurl should be fine for fetchurl, but not fetchgit)
    srcExpr = Fix $ NBinary NApp (fromMaybe (defaultFetcherFn) originalFetcherFn) fetcherArgs
    layoutOpts = Doc.defaultLayoutOptions { Doc.layoutPageWidth = Doc.AvailablePerLine 1 1.0}
    prettyPrint = Doc.renderStrict . Doc.removeTrailingWhitespace . Doc.layoutPretty layoutOpts
    srcDoc = Pretty.prettyNix srcExpr
    srcTextRaw = fixupFinalLine $ prettyPrint $ Doc.nest (tabIndent + spaceIndent) srcDoc
      where
        tabIndent = nixIndentWidth * (tabs leadingIndent)
        spaceIndent = (spaces leadingIndent)
    srcText = T.intercalate "\n" (replaceLeadingIndents (T.lines srcTextRaw))

    replaceLeadingIndents lines = map replaceLeadingIndent lines
    replaceLeadingIndent = if tabs leadingIndent == 0 then id else injectTabs

    leadingIndent = Indent { tabs = T.length tabs, spaces = T.length spaces }
      where
        (tabs, afterTabs) = T.span isTab partialStart
        (spaces, _) = T.span isSpace afterTabs

    injectTabs line = indent <> text
      where
        (leadingSpaces, text) = T.span isSpace line
        indentWidth = T.length leadingSpaces
        numTabs = indentWidth `quot` nixIndentWidth
        numSpaces = indentWidth - (nixIndentWidth * numTabs)
        indent = (T.replicate numTabs "\t") <> (T.replicate numSpaces " ")

    -- hnix pretty-print aligns closing brace with attributes, which looks weird.
    -- This always operates on pretty-printed nix, so indent is 2 spaces
    fixupFinalLine srcText = fixupFinalLine' (T.lines srcText)
      where
        fixupFinalLine' [] = ""
        fixupFinalLine' [last] = reduceIndent last
        fixupFinalLine' (line : tail) = line <> "\n" <> (fixupFinalLine' tail)
      
        reduceIndent line = newSpaces <> remainder where
          newSpaces = T.drop nixIndentWidth leadingSpaces
          (leadingSpaces, remainder) = T.span isSpace line

extractSourceLocs :: Fix NExprLocF -> [(Maybe (Fix NExprF), SrcSpan)]
extractSourceLocs expr =
  foldMap extractSources (unFix expr) where
    dbg :: String -> a -> a
    dbg s x = unsafePerformIO (putStrLn s >> return x)
    extractSources :: Fix NExprLocF -> [(Maybe (Fix NExprF), SrcSpan)]
    extractSources expr =
      case value of
        (NSet bindings) -> dbg "NSet bindings" $ extractSourceBindings bindings
        (NRecSet bindings) -> dbg "NRecSet bindings" $ extractSourceBindings bindings
        other -> foldMap extractSources other -- dbg ("nothing interesting -- " ++ (show other)) []
      where
        node = (getCompose . unFix) expr
        value = annotated node

    extractSourceBindings :: [Binding (Fix NExprLocF)] -> [(Maybe (Fix NExprF), SrcSpan)]
    extractSourceBindings bindings = concatMap apply bindings where
      apply :: Binding (Fix NExprLocF) -> [(Maybe (Fix NExprF), SrcSpan)]
      apply (NamedVar ((StaticKey "src") :| []) value _pos) =
        (extractSourceFn value, sourcePos) : extractSourceLocs value
        where
          sourcePos = (annotation . getCompose . unFix) value
      apply _ = dbg "non-srv binding" []

    extractSourceFn :: Fix NExprLocF -> Maybe (Fix NExprF)
    extractSourceFn expr = case nexpr of
      (NBinary NApp fn _args) -> Just (Expr.stripAnnotation fn)
      _ -> Nothing
      where
        nexpr = annotated . getCompose . unFix $ expr


