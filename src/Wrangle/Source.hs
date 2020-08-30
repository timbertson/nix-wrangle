{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Wrangle.Source where

import Prelude hiding (error)
import Control.Monad
import Control.Exception (Exception)
import Data.Bifoldable (bifoldMap)
import Control.Applicative ((<|>))
import Data.Aeson hiding (eitherDecodeFileStrict, encodeFile)
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Hashable (Hashable)
import System.FilePath ((</>))
import Wrangle.Util
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Options.Applicative as Opts
import qualified System.Directory as Dir

{-
 - Terminology:
 -
 - SourceFile : a wrangle(/-local).json file
 - Packages: the loaded contents of a SourceFile, as a map of PackageName -> PackageSpec
 - PackageSpec: an entry in Packages
 - SourceSpec: an (impure) fetch spec for some source code. Resolved to specific `fetch` information on update.
-}

latestApiVersion = 1

data FetchAttr = S String | B Bool deriving Eq
instance Show FetchAttr where
  show (S x) = show x
  show (B x) = show x

type FetchKV = (String, FetchAttr)

type StringMap = HMap.HashMap String String

type FetchMap = HMap.HashMap String FetchAttr

fetchKeyJSON = "fetch" :: T.Text
typeKeyJSON = "type" :: T.Text
versionKeyJSON = "version" :: T.Text
sourcesKeyJSON = "sources" :: T.Text
wrangleKeyJSON = "wrangle" :: T.Text
apiversionKeyJSON = "apiversion" :: T.Text
fetchSubmodulesKeyJSON = "fetchSubmodules" :: String

wrangleHeaderJSON :: Aeson.Value
wrangleHeaderJSON =
  Object $ HMap.singleton apiversionKeyJSON (toJSON latestApiVersion)

-- TODO rename, obviously...
class ToStringPairs t where
  toStringPairs :: t -> [FetchKV]
  toStringMap :: t -> FetchMap
  toStringMap = HMap.fromList . toStringPairs

class AsString t where
  asString :: t -> String

newtype Template = Template String deriving (Show, Eq, FromJSON, ToJSON)

instance AsString Template where
  asString (Template s) = s

newtype GitRevision = GitRevision String deriving (Show, Eq, FromJSON, ToJSON)

instance AsString GitRevision where
  asString (GitRevision s) = s

newtype Sha256 = Sha256 String deriving (Show, Eq, FromJSON, ToJSON)

instance AsString Sha256 where
  asString (Sha256 s) = s

data GitCommon = GitCommon {
  fetchSubmodules :: Bool
} deriving (Show, Eq)

defaultGitCommon = GitCommon { fetchSubmodules = False }

data FetchType
  = FetchGithub
  | FetchGit
  | FetchUrl UrlFetchType
  | FetchGitLocal
  | FetchPath

allFetchTypes = [ FetchGithub, FetchGit, FetchUrl FetchArchive, FetchUrl FetchFile, FetchGitLocal, FetchPath ]
validTypesDoc = "Valid types: " <> (intercalate "|" $ map fetcherNameWrangle allFetchTypes)

parseFetchType :: String -> Either String FetchType
parseFetchType s = case s of
  "github" -> Right FetchGithub
  "url" -> Right $ FetchUrl FetchArchive
  "file" -> Right $ FetchUrl FetchFile
  "path" -> Right FetchPath
  "git-local" -> Right FetchGitLocal
  t -> Left $ "Unsupported type: '" <> t <> "'. "<>validTypesDoc

fetchType :: SourceSpec -> FetchType
fetchType spec = case spec of
  (Github _) -> FetchGithub
  (Url (UrlSpec { urlType })) -> FetchUrl urlType
  (Git _) -> FetchGit
  (GitLocal _) -> FetchGitLocal
  (Path _) -> FetchPath

fetcherNameNix :: FetchType -> Either AppError String
fetcherNameNix f = case f of
    FetchGithub -> Right "fetchFromGitHub"
    FetchGit -> Right "fetchgit"
    (FetchUrl FetchArchive) ->  Right "fetchzip"
    (FetchUrl FetchFile) ->  Right "fetchurl"
    -- These two don't have a nix fetcher, which means they
    -- aren't supported in `splice`, but they also require
    -- no prefetching
    FetchGitLocal -> err
    FetchPath -> err
    where err = Left $ AppError $ "No plain-nix fetcher for type '"<>fetcherNameWrangle f<>"'"

fetcherNameWrangle :: FetchType -> String
fetcherNameWrangle f = case f of
    FetchGithub -> "github"
    FetchGit -> "git"
    FetchGitLocal -> "git-local"
    FetchPath -> "path"
    (FetchUrl typ) ->  asString typ

data GithubSpec = GithubSpec {
  ghOwner :: String,
  ghRepo :: String,
  ghRef :: Template,
  ghCommon :: GitCommon
} deriving (Show, Eq)

instance ToStringPairs GithubSpec where
  toStringPairs GithubSpec { ghOwner, ghRepo, ghRef } =
    [
      ("owner", S ghOwner),
      ("repo", S ghRepo),
      ("ref", S $ asString ghRef)
    ]
    
instance FromJSON FetchAttr where
  parseJSON (Bool v) = pure (B v)
  parseJSON (String v) = pure (S (T.unpack v))
  parseJSON v = typeMismatch "String/Boolean" v
  
instance ToJSON FetchAttr where
  toJSON (B v) = toJSON v
  toJSON (S v) = toJSON v

data UrlFetchType = FetchArchive | FetchFile deriving (Show, Eq)

instance AsString UrlFetchType where
  asString FetchArchive = "url"
  asString FetchFile = "file"

instance ToJSON UrlFetchType where
  toJSON = toJSON . asString

instance FromJSON UrlFetchType where
  parseJSON (String "file") = pure FetchFile
  parseJSON (String "url") = pure FetchArchive
  parseJSON v = typeMismatch "\"file\" or \"url\"" v

data UrlSpec = UrlSpec {
  urlType :: UrlFetchType,
  url :: Template
} deriving (Show, Eq)

instance ToStringPairs UrlSpec where
  toStringPairs UrlSpec { urlType = _urlType, url } =
    [ ("url", S $ asString url) ]

data GitSpec = GitSpec {
  gitUrl :: String,
  gitRef :: Template,
  gitCommon :: GitCommon
} deriving (Show, Eq)

instance ToStringPairs GitSpec where
  toStringPairs GitSpec { gitUrl, gitRef } =
    [
      ("url", S gitUrl),
      ("ref", S $ asString gitRef)
    ]

data LocalPath
  = FullPath FilePath
  | RelativePath FilePath
   deriving (Show, Eq)

instance ToStringPairs LocalPath where
  toStringPairs (FullPath p) = [("path", S p)]
  toStringPairs (RelativePath p) = [("relativePath", S p)]

data GitLocalSpec = GitLocalSpec {
  glPath :: LocalPath,
  glRef :: Maybe Template,
  glCommon :: GitCommon
} deriving (Show, Eq)

instance ToStringPairs GitLocalSpec where
  toStringPairs GitLocalSpec { glPath, glRef } =
    (toStringPairs glPath) <> optList (refAttr <$> glRef) where
      refAttr ref = ("ref", S $ asString ref)

data SourceSpec
  = Github GithubSpec
  | Url UrlSpec
  | Git GitSpec
  | GitLocal GitLocalSpec
  | Path LocalPath
  deriving (Show, Eq)

instance ToStringPairs SourceSpec where
  toStringPairs (Github f) = toStringPairs f
  toStringPairs (Url f) = toStringPairs f
  toStringPairs (Git f) = toStringPairs f
  toStringPairs (GitLocal f) = toStringPairs f
  toStringPairs (Path f) = toStringPairs f

parseBoolFromString :: String -> Parser Bool
parseBoolFromString "true" = pure True
parseBoolFromString "false" = pure False
parseBoolFromString other = fail $ "Not a boolean: " <> other

-- TODO return (SourceSpec, remainingAttrs)?
parseSourceSpecObject :: Value -> Object -> Parser SourceSpec
parseSourceSpecObject fetcher attrs = parseFetcher fetcher >>= parseSpec
  where
    parseFetcher :: Value -> Parser FetchType
    parseFetcher json = parseJSON json >>= bifoldMap invalid pure . parseFetchType

    parseSpec :: FetchType -> Parser SourceSpec
    parseSpec fetcher = case fetcher of
      FetchGithub ->
        build <$> owner <*> repo <*> refRequired <*> gitCommon where
          build ghOwner  ghRepo  ghRef  ghCommon = Github $ GithubSpec {
                ghOwner, ghRepo, ghRef, ghCommon }
      FetchGit -> build <$> url <*> refRequired <*> gitCommon where
        build gitUrl gitRef gitCommon = Git $ GitSpec { gitUrl, gitRef, gitCommon }
      FetchGitLocal ->
        build <$> path <*> refOpt <*> gitCommon where
          build glPath glRef glCommon = GitLocal $ GitLocalSpec { glPath, glRef, glCommon }
      FetchPath -> Path <$> path
      (FetchUrl t) -> buildUrl t <$> url

    gitCommon :: Parser GitCommon
    gitCommon = build <$> fetchSubmodulesOpt where
      build fetchSubmodules = GitCommon { fetchSubmodules }

    owner = attrs .: "owner"
    repo = attrs .: "repo"
    url = attrs .: "url"
    path = (FullPath <$> attrs .: "path") <|> (RelativePath <$> attrs .: "relativePath")
    refRequired :: Parser Template = attrs .: "ref"
    refOpt :: Parser (Maybe Template) = attrs .:? "ref"

    fetchSubmodulesOpt :: Parser Bool
    fetchSubmodulesOpt = do
      (strOpt :: Maybe String) <- attrs .:? (T.pack fetchSubmodulesKeyJSON)
      (boolOpt :: Maybe Bool) <- traverse parseBoolFromString strOpt
      return $ maybe (fetchSubmodules defaultGitCommon) id boolOpt

    buildUrl urlType url = Url $ UrlSpec { urlType, url = Template url }
    invalid v = fail $ "Unable to parse SourceSpec from: " <> (encodePrettyString v)

data PackageSpec = PackageSpec {
  sourceSpec :: SourceSpec,
  fetchAttrs :: FetchMap,
  packageAttrs :: StringMap
} deriving (Show, Eq)

instance FromJSON PackageSpec where
  parseJSON = withObject "PackageSpec" (\attrs -> do
    (fetchJSON, attrs) <- attrs `extract` fetchKeyJSON
    (fetcher, attrs) <- attrs `extract` typeKeyJSON
    let fetchAttrs = (parseJSON fetchJSON)
    let packageAttrs = (parseJSON (Object attrs))
    build <$> (parseSourceSpecObject fetcher attrs)
      <*> fetchAttrs <*> packageAttrs
    ) where
    extract obj key = pairWithout key obj <$> obj .: key
    pairWithout key obj v = (v, HMap.delete key obj)
    build sourceSpec fetchAttrs packageAttrs = PackageSpec {
      sourceSpec, fetchAttrs, packageAttrs
    }

instance ToJSON PackageSpec where
  toJSON PackageSpec { sourceSpec, fetchAttrs, packageAttrs } =
    toJSON
      . HMap.insert (T.unpack typeKeyJSON) (toJSON . fetcherNameWrangle . fetchType $ sourceSpec)
      . HMap.insert (T.unpack fetchKeyJSON) (toJSON fetchAttrs)
      $ (HMap.map toJSON (HMap.map S packageAttrs <> toStringMap sourceSpec))

newtype Packages = Packages
  { unPackages :: HMap.HashMap PackageName PackageSpec }
  deriving newtype (Show)

emptyPackages = Packages HMap.empty

add :: Packages -> PackageName -> PackageSpec -> Packages
add s name spec = Packages $ HMap.insert name spec $ unPackages s

remove :: Packages -> PackageName -> Packages
remove s name = Packages $ HMap.delete name $ unPackages s

instance FromJSON Packages where
  parseJSON = withObject "document" $ \obj ->
    (obj .: wrangleKeyJSON >>= checkHeader) >>
    Packages <$> (obj .: sourcesKeyJSON >>= withObject "sources" parsePackageSpecs)
    where
      parsePackageSpecs attrs = HMap.fromList <$> mapM parseItem (HMap.toList attrs)
      parseItem :: (T.Text, Value) -> Parser (PackageName, PackageSpec)
      parseItem (k,v) = (PackageName $ T.unpack k,) <$> parseJSON v
      checkHeader = withObject "Wrangle Header" $ \obj ->
        (obj .: apiversionKeyJSON >>= checkApiVersion)
      checkApiVersion v =
        if v == (Number latestApiVersion)
          then pure ()
          else fail ("unsupported API version: " <> (TL.unpack . TLE.decodeUtf8 $ Aeson.encode v))

instance ToJSON Packages where
  toJSON (Packages s) = toJSON $
    HMap.fromList [
      (sourcesKeyJSON, toJSON s),
      (wrangleKeyJSON, wrangleHeaderJSON)
    ]

liftResult :: Result a -> Either AppError a
liftResult (Error err) = Left $ AppError err
liftResult (Success x) = Right x

-- Note: doesn't update `fetch` attrs
updatePackageSpec :: PackageSpec -> StringMap -> Either AppError PackageSpec
updatePackageSpec original attrs = mergedJSON >>= liftResult <$> fromJSON where
  -- Going via JSON is a little hacky, but
  -- we've already got a nice to/from JSON code path
  mergedJSON = case (toJSON original, toJSON attrs) of
    (Object orig, Object add) -> Right $ Object $ HMap.union add orig
    (_, _) -> Left $ "Expected JSON object" -- should be impossible

loadSourceFile :: SourceFile -> IO Packages
loadSourceFile source = do
  debugLn $ "Reading sources: " ++ sourcePath
  contents <- eitherDecodeFileStrict sourcePath
  liftEither $ mapLeft invalidSourceDocument contents
  where
    sourcePath = pathOfSource source
    invalidSourceDocument reason = AppError $ unlines [
      "Cannot load " <> sourcePath,
      "This file should be a JSON map with toplevel objects `sources` and `wrangle`.",
      reason]

loadSources :: NonEmpty SourceFile -> IO (NonEmpty Packages)
loadSources sources = do
  debugLn $ "Loading sources: " ++ (show sources)
  traverse loadSourceFile sources

merge :: NonEmpty Packages -> Packages
merge packages = do
  -- TODO check order
  Packages $ foldr HMap.union HMap.empty (unPackages <$> packages)

writeSourceFile :: SourceFile -> Packages -> IO ()
writeSourceFile sourceFile packages =
  encodeFile (pathOfSource sourceFile) packages

newtype NotFound = NotFound (String, [String])
instance Show NotFound where
  show (NotFound (key, keys)) =
    "key `" <> key <> "` not found in " <> (show keys)

instance Exception NotFound

lookup :: PackageName -> Packages -> Either NotFound PackageSpec
lookup pkg (Packages packages) = toRight err (HMap.lookup pkg packages) where
  err = (NotFound (show pkg, asString <$> HMap.keys packages))

keys :: Packages -> [PackageName]
keys = HMap.keys . unPackages

member :: Packages -> PackageName -> Bool
member = (flip HMap.member) . unPackages

defaultSourceFileCandidates :: [SourceFile]
defaultSourceFileCandidates = [ DefaultSource, LocalSource ]

doesSourceExist = Dir.doesFileExist . pathOfSource

detectDefaultSources :: IO (Maybe (NonEmpty SourceFile))
detectDefaultSources = tap log $ NonEmpty.nonEmpty <$> fileList where
  log sources = debugLn $ "Detected default sources:" <> show sources
  fileList = filterM doesSourceExist defaultSourceFileCandidates

configuredSources :: Maybe (NonEmpty SourceFile) -> IO (Maybe (NonEmpty SourceFile))
configuredSources Nothing = detectDefaultSources
configuredSources explicitSources@(Just _) = return explicitSources

newtype PackageName = PackageName { unPackageName :: String }
  deriving newtype (Eq, Hashable, FromJSONKey, ToJSONKey, Show)

instance AsString PackageName where asString = unPackageName

parsePackageName :: Opts.Parser PackageName
parsePackageName = PackageName <$>
    Opts.argument Opts.str (Opts.metavar "PACKAGE")

eitherDecodeFileStrict :: (FromJSON a) => FilePath -> IO (Either String a)
eitherDecodeFileStrict = fmap Aeson.eitherDecodeStrict . B.readFile

encodePretty :: (ToJSON a) => a -> L.ByteString
encodePretty = AesonPretty.encodePretty' (AesonPretty.defConfig {
  AesonPretty.confIndent = AesonPretty.Spaces 2,
  AesonPretty.confCompare = AesonPretty.compare
})

encodePrettyString :: (ToJSON a) => a -> String
encodePrettyString = stringOfLazy . encodePretty

encodeOnelineString :: (ToJSON a) => a -> String
encodeOnelineString = stringOfLazy . Aeson.encode

stringOfLazy :: L.ByteString -> String
stringOfLazy = TL.unpack . TLE.decodeUtf8

encodeFile :: (ToJSON a) => FilePath -> a -> IO ()
encodeFile path json = writeFileLazyBytestring path (encodePretty json <> "\n")

writeFileLazyBytestring :: FilePath -> L.ByteString -> IO ()
writeFileLazyBytestring path contents = do
  debugLn $ "Writing contents: " <> (show contents)
  writeFileContents' L.writeFile path contents

writeFileText :: FilePath -> T.Text -> IO ()
writeFileText path text = do
  debugLn $ "Writing contents: " <> (show text)
  writeFileContents' (\path -> B.writeFile path . E.encodeUtf8) path text

writeFileContents' :: (FilePath -> a -> IO ()) -> FilePath -> a -> IO ()
writeFileContents' writer path contents = do
  infoLn $ "Writing: " <> path
  writer tmpPath contents
  Dir.renameFile tmpPath path
  where tmpPath = path <> ".tmp" :: FilePath

data SourceFile
  = DefaultSource
  | LocalSource
  | NamedSource String
  deriving Show

pathOfSource :: SourceFile -> FilePath
pathOfSource source = case source of
  DefaultSource -> "nix" </> "wrangle.json"
  LocalSource -> "nix" </> "wrangle-local.json"
  NamedSource path -> path
