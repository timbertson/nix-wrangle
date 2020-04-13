{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Wrangle.Fetch where

import Prelude hiding (error)
import Control.Exception (toException)
import Control.Monad.Except (throwError)
import Control.Applicative (liftA2)
import Data.Aeson (toJSON)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Char (isSpace)
import Text.Regex.TDFA
import System.Environment (lookupEnv, getExecutablePath)
import System.FilePath.Posix ((</>), takeDirectory)
import Wrangle.Util
import Wrangle.Source
import qualified System.IO.Strict as H
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified System.Process as P
import qualified System.Directory as Dir

prefetch :: PackageName -> PackageSpec -> IO PackageSpec
prefetch name pkg = do
  infoLn $ "fetching " <> (show src)
  fetchAttrs <- HMap.fromList <$> resolveAttrs src
  debugLn $ "Prefetch results: " <> show fetchAttrs
  return $ pkg { fetchAttrs = fetchAttrs }
  where

  digestKey = "sha256"
  existing key = HMap.lookup key (fetchAttrs pkg)
  src = sourceSpec pkg
  render = renderTemplate (packageAttrs pkg)

  updateDigestIf :: Bool -> [String] -> [(String,String)] -> IO [(String,String)]
  updateDigestIf cond path attrs = case existing digestKey of
    (Just digest) | not cond -> return $ (digestKey, digest) : attrs
    _ -> addDigest path attrs

  addDigest :: [String] -> [(String,String)] -> IO [(String,String)]
  addDigest path attrs = prefix <$> (log $ prefetchSha256 (fetchType src) attrs) where
    prefix d = (digestKey, asString d) : attrs
    log = tap (\d -> do
      infoLn $ "Resolved " <> (intercalate " -> " (asString name : path))
      infoLn $ " - "<>digestKey<>":" <> asString d)

  resolveGit :: String -> Template -> IO (String, GitRevision)
  resolveGit repo ref = do
    ref <- liftEither $ render ref
    commit <- revision <$> resolveGitRef repo ref
    return (ref, commit)
      
  addGitDigest :: String -> GitRevision -> [(String,String)] -> IO [(String,String)]
  addGitDigest ref commit =
    updateDigestIf (Just (asString commit) /= existing "rev") [ref, asString commit]

  resolveAttrs :: SourceSpec -> IO [(String,String)]
  resolveAttrs (Github (GithubSpec { ghOwner, ghRepo, ghRef })) = do
    (ref, commit) <- resolveGit repo ghRef
    addGitDigest ref commit [
      ("owner", ghOwner),
      ("repo", ghRepo),
      ("rev", asString commit)]
    where repo = "https://github.com/"<>ghOwner<>"/"<>ghRepo<>".git"

  resolveAttrs (Git (GitSpec { gitUrl, gitRef })) = do
    (ref, commit) <- resolveGit gitUrl gitRef
    addGitDigest ref commit [("url", gitUrl), ("rev", asString commit)]

  resolveAttrs (Url (UrlSpec { url })) = do
    renderedUrl <- liftEither $ render url
    addDigest [renderedUrl] [("url", renderedUrl)]

  -- *Local require no prefetching:
  resolveAttrs (GitLocal (GitLocalSpec { glPath, glRef })) = do
    ref <- liftEither $ sequence $ render <$> glRef
    return $ optList (refAttr <$> ref) <> toStringPairs glPath
    where
      refAttr ref = ("ref", ref)

  resolveAttrs (Path p) = do
    return $ toStringPairs p

data ResolvedRef = ResolvedRef {
  revision :: GitRevision,
  ref :: String
} deriving Show

data OutputStream = Stdout | Stderr

runProcessOutput :: OutputStream -> P.CreateProcess -> IO String
runProcessOutput src p = P.withCreateProcess p read where
  read _stdin stdout stderr proc = do
    h <- liftMaybe (toException $ AppError $ srcDesc<>" handle is null") handle
    contents <- H.hGetContents h
    _ <- P.waitForProcess proc
    return contents
    where
      (srcDesc, handle) = case src of
        Stdout -> ("stdout", stdout)
        Stderr -> ("stderr", stderr)

-- Git ref is resolved though:
-- $ git ls-remote ~/dev/ocaml/gup/
-- $ git ls-remote https://github.com/timbertson/gup.git
-- Output: lines of `SHA<tab>ref`
resolveGitRef :: String -> String -> IO ResolvedRef
resolveGitRef remote refName = do
  debugLn $ "Resolving git reference: "<>showRef
  refs <- getRefs
  sequence_ $ map (debugLn . show) refs
  tap logResult $ liftEither $ toRight missing $ firstMatch (refs <> identityRef)
  where
    showRef = remote<>"#"<>refName
    missing = AppError $ "Couldn't resolve ref "<>showRef
    logResult result = debugLn $ "Resolved to: "<> show result

    identityRef =
      if isValidSha
      then [ResolvedRef {
          revision = GitRevision refName,
          ref = refName
        }]
      else []
      where
        shaRegex = "^[0-9a-fA-F]{40}$" :: String
        isValidSha = refName =~ shaRegex :: Bool

    getRefs :: IO [ResolvedRef]
    getRefs = do
      lines <- T.lines . T.pack <$> runProcessOutput Stdout processSpec
      return $ mapMaybe parseLine lines

    processSpec = (P.proc "git" ["ls-remote", remote]) {
      P.std_in = P.NoStream,
      P.std_out = P.CreatePipe,
      P.std_err = P.Inherit
    }

    parseLine :: T.Text -> Maybe ResolvedRef
    parseLine line = if rev == ""
      then Nothing
      else Just (ResolvedRef { revision = GitRevision (T.unpack rev), ref = T.unpack ref })
      where
        (rev, remainder) = T.break isSpace line
        ref = T.strip remainder

    firstMatch refs = listToMaybe (matchingRefs refs)
    matchingRefs :: [ResolvedRef] -> [ResolvedRef]
    matchingRefs refs = concatMap (\c -> filter (matches c) refs) candidates where
      candidates = [refName, "refs/tags/"<>refName, "refs/heads/"<>refName]
      matches :: String -> ResolvedRef -> Bool
      matches candidate = ((==) candidate) . ref

shaLen = 52
dummySHA256 = concat $ replicate shaLen "0"

nixBuildCommand :: NixApiContext -> FetchType -> [(String,String)] -> NonEmpty String
nixBuildCommand (NixApiContext { apiNix, projectRoot }) fetchType attrs
  = exe :| args
  where
    fetcherName = fetcherNameWrangle fetchType
    fetchJSON = encodeOnelineString . toJSON . HMap.fromList $ attrs
    fetchExpr = intercalate "\n" [
      "{fetchJSON, apiPath, path}:",
      "let api = (import <nixpkgs> {}).callPackage apiPath {}; in",
      "(api.internal.makeFetchers { inherit path; })."<>fetcherName<>" (builtins.fromJSON fetchJSON)"]
    exe = "nix-build"
    args = [
      "--no-out-link",
      "--show-trace",
      "--argstr", "fetchJSON", fetchJSON,
      "--argstr", "path", projectRoot,
      "--argstr", "apiPath", apiNix,
      "--expr", fetchExpr]

data NixApiContext = NixApiContext {
  apiNix :: FilePath,
  projectRoot :: FilePath
}

globalApiContext :: IO NixApiContext
globalApiContext = do
  declaredDataDir <- lookupEnv "NIX_WRANGLE_DATA"
  dataDir <- case declaredDataDir of
    (Just dir) -> return dir
    Nothing -> (\base -> (takeDirectory base) </> ".." </> ".." </> "..") <$> getExecutablePath
  debugLn $ "using NIX_WRANGLE_DATA directory " <> dataDir
  let apiNix = dataDir </> "nix" </> "api.nix"
  projectRoot <- Dir.getCurrentDirectory
  return $ NixApiContext { apiNix, projectRoot }

-- This supports arbitrary prefetching without worrying about nix-prefetch-*.
-- It's slightly inefficient since it results in two downloads of a file,
-- but it's very reliable regardless of fetch method.
prefetchSha256 :: FetchType -> [(String,String)] -> IO Sha256
prefetchSha256 fetchType attrs = do
  apiContext <- globalApiContext
  let cmd = nixBuildCommand apiContext fetchType $ ("sha256", dummySHA256) : attrs
  debugLn $ "+ " <> (show $ NonEmpty.toList cmd)
  errText <- runProcessOutput Stderr (processSpec cmd)
  sequence_ $ map debugLn $ lines errText
  liftEither $ extractExpectedDigest errText
  where
    processSpec cmd = (P.proc (NonEmpty.head cmd) (NonEmpty.tail cmd)) {
      P.std_in = P.NoStream,
      P.std_out = P.NoStream,
      P.std_err = P.CreatePipe }

-- Thanks https://github.com/seppeljordan/nix-prefetch-github/blob/cd9708fcdf033874451a879ac5fe68d7df930b7e/src/nix_prefetch_github/__init__.py#L124
-- For the future, note SRI: https://github.com/NixOS/nix/commit/6024dc1d97212130c19d3ff5ce6b1d102837eee6
-- and https://github.com/NixOS/nix/commit/5e6fa9092fb5be722f3568c687524416bc746423
extractExpectedDigest :: String -> Either AppError Sha256
extractExpectedDigest output = Sha256 <$> (
  (singleResult $ subMatches nix_1_x) `orTry`
  (singleResult $ subMatches nix_2_0) `orTry`
  (singleResult $ subMatches nix_2_2) `orTry`
  (singleResult $ filter (/= dummySHA256) $ subMatches fallback))
  where
  subMatches :: String -> [String]
  subMatches pat = concat $ drop 1 <$> ((output =~ pat) :: [[String]])

  shaRe = "([a-z0-9]{"<> (show shaLen) <>"})"

  nix_1_x = "output path .* has .* hash '"<>shaRe<>"' when .*"
  nix_2_0 = "fixed-output derivation produced path .* with sha256 hash '"<>shaRe<>"' instead of the expected hash .*"
  nix_2_2 = "  got: +sha256:"<>shaRe
  fallback = shaRe

  singleResult [result] = Right result
  singleResult _ = Left . AppError $ "Unable to detect resulting digest from nix-build output:\n\n" <> output

renderTemplate :: StringMap -> Template -> Either AppError String
renderTemplate attrs fullText = render (asString fullText) where
  render :: String -> Either AppError String
  render ('<':str) = do
      case span (/= '>') str of
        (key, '>':rest) ->
          liftA2 (<>) value (render rest)
          where
            value = toRight notFound $ HMap.lookup key attrs
            notFound = AppError $ "Missing key `"<> key <>"` in template: " <> (asString fullText)
        _ -> throwError . AppError $ "Value contains an unterminated key: " <> (asString fullText)
  render (c:str) = (c:) <$> render str
  render [] = Right []
