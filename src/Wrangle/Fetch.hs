{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Wrangle.Fetch where

import Prelude hiding (error)
-- import Control.Monad
import Control.Exception (toException)
-- import Control.Error.Safe (justErr)
import Control.Monad.Except (throwError)
import Control.Applicative ((<|>), liftA2)
import Data.Aeson (toJSON)
-- import Data.Aeson.Types (typeMismatch, Parser)
-- import Data.Hashable (Hashable)
-- import System.FilePath ((</>))
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.List (intercalate)
import Wrangle.Util
import Wrangle.Source
import Text.Regex.TDFA
import qualified GitHub as GH
import qualified GitHub.Data.Name as GH
import qualified GitHub.Data.GitData as GHData
import qualified GitHub.Endpoints.GitData.References as GHRef
import qualified GHC.IO.Handle as H
-- import qualified Data.Aeson as Aeson
-- import qualified Data.Aeson.Encode.Pretty as AesonPretty
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified System.Process as P
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Encoding as TLE
-- import qualified Options.Applicative as Opts
-- import qualified System.Directory as Dir
-- import qualified System.FilePath.Posix as PosixPath

prefetch :: PackageName -> PackageSpec -> IO PackageSpec
prefetch name pkg = do
  debugLn ("fetching " <> (show src))
  fetchAttrs <- HMap.fromList <$> resolveAttrs src
  debugLn $ "Prefetch results: " <> show fetchAttrs
  return $ pkg { fetchAttrs = fetchAttrs }
  where

  src = sourceSpec pkg
  render = renderTemplate (packageAttrs pkg)

  addDigest :: [String] -> [(String,String)] -> IO [(String,String)]
  addDigest path attrs = prefix <$> (log $ prefetchSha256 (fetcherName src) attrs) where
    prefix d = ("sha256", asString d) : attrs
    log = tap (\d -> do
      infoLn $ "Resolved " <>
        (intercalate " -> " (asString name : path))
      infoLn $ " - sha256-" <> (asString d))

  resolveAttrs :: SourceSpec -> IO [(String,String)]
  resolveAttrs (Github (spec@ GithubSpec { ghOwner, ghRepo, ghRef })) = do
    ref <- liftEither $ render ghRef
    commit <- resolveGithubRef spec ref
    addDigest [ref, asString commit] [
      ("owner", ghOwner),
      ("repo", ghRepo),
      ("rev", (asString commit))]

  resolveAttrs (Url (UrlSpec { url })) = do
    renderedUrl <- liftEither $ render url
    addDigest [renderedUrl] [("url", renderedUrl)]

  resolveAttrs (Git (GitSpec { gitUrl, gitRef })) = do
    ref <- liftEither $ render gitRef
    commit <- resolveGitRef gitUrl ref
    addDigest [ref, asString commit] [("url", gitUrl), ("rev", asString commit)]

  resolveAttrs (GitLocal (GitLocalSpec { glPath, glRef })) = do
    -- no prefetching necessary!
    ref <- liftEither $ render glRef
    return [("ref", ref), ("path", asString glPath)]

resolveGithubRef :: GithubSpec -> String -> IO GitRevision
resolveGithubRef (GithubSpec { ghRepo, ghOwner }) ref = do
  debugLn $ "Resolving github reference: "<>ghRepo<>"/"<>ghOwner<>"#"<>ref
  resolveGithubRef' ref <$> refs
  where
    refs = GHRef.references (name ghOwner) (name ghRepo) >>= liftEither
    name = GH.N . T.pack

-- Git ref could be resolved though:
--  - smart API, e.g. https://github.com/schacon/simplegit.git/info/refs?service=git-upload-pack
--  - ssh, e.g. ssh git@github.com git-upload-pack schacon/simplegit.git
--  - local path, e.g. `git upload-pack /path/to/DIR`
resolveGitRef :: String -> String -> IO GitRevision
resolveGitRef _url _ref = fail "TODO"

resolveGithubRef' :: String -> Vector GH.GitReference -> GitRevision
resolveGithubRef' ref refs = extractCommit (
    nameEq ref <|> nameEq ("refs/tags/"<>ref) <|> nameEq ("refs/heads/"<>ref)
  )
  where
    nameEq :: String -> Maybe GH.GitReference
    nameEq candidate = Vector.find ((== T.pack candidate) . GHData.gitReferenceRef) refs
    extractCommit Nothing = GitRevision ref -- assume it's a commit
    extractCommit (Just ref) = GitRevision . T.unpack . GHData.gitObjectSha . GHData.gitReferenceObject $ ref


shaLen = 52
dummySHA256 = concat $ replicate shaLen "0"

-- This supports arbitrary prefetching without worrying about nix-prefetch-*.
-- It's slightly inefficient since it retults in two downloads of a file,
-- but it's very reliable regardless of fetch method.
prefetchSha256 :: FetcherName -> [(String,String)] -> IO Sha256
prefetchSha256 fetcher attrs = do
  debugLn $ "prefetching "<>(wrangleName fetcher) <> " digest"
  debugLn $ "+ " <> (show $ exe : args)
  P.withCreateProcess processSpec parseErr
  where
    fetchExpr = intercalate " " [
      "{fetchJSON}:",
      "(import <nixpkgs> {})." <> (nixName fetcher),
      "(builtins.fromJSON fetchJSON)"]
    fetchJSON = encodeOnelineString . toJSON . HMap.fromList $ ("sha256", dummySHA256) : attrs
    exe = "nix-build"
    args = [
      "--no-out-link",
      "--argstr", "fetchJSON", fetchJSON,
      "--expr", fetchExpr]

    parseErr _ _ maybeErr proc = do
      errHandle <- liftMaybe (toException $ AppError "stderr handle is null") maybeErr
      errText <- H.hGetContents errHandle
      sequence_ $ map debugLn $ lines errText
      _ <- P.waitForProcess proc
      liftEither $ extractExpectedDigest errText

    processSpec = (P.proc exe args) {
      P.std_out = P.NoStream,
      P.std_in = P.NoStream,
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
