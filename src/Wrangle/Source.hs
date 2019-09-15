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

type StringMap = HMap.HashMap String String

fetchKeyJSON = "fetch" :: T.Text
typeKeyJSON = "type" :: T.Text
versionKeyJSON = "version" :: T.Text
sourcesKeyJSON = "sources" :: T.Text
wrangleKeyJSON = "wrangle" :: T.Text
apiversionKeyJSON = "apiversion" :: T.Text
wrangleHeaderJSON :: Aeson.Value
wrangleHeaderJSON =
  Object $ HMap.singleton apiversionKeyJSON (toJSON latestApiVersion)

class ToStringPairs t where
  toStringPairs :: t -> [(String, String)]
  toStringMap :: t -> StringMap
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

data FetchType =
  FetchGithub
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
  ghRef :: Template
} deriving (Show, Eq)

instance ToStringPairs GithubSpec where
  toStringPairs GithubSpec { ghOwner, ghRepo, ghRef } =
    [
      ("owner", ghOwner),
      ("repo", ghRepo),
      ("ref", asString ghRef)
    ]

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
    [ ("url", asString url) ]

data GitSpec = GitSpec {
  gitUrl :: String,
  gitRef :: Template
} deriving (Show, Eq)

instance ToStringPairs GitSpec where
  toStringPairs GitSpec { gitUrl, gitRef } =
    [
      ("url", gitUrl),
      ("ref", asString gitRef)
    ]

data LocalPath
  = FullPath FilePath
  | RelativePath FilePath
   deriving (Show, Eq)

instance ToStringPairs LocalPath where
  toStringPairs (FullPath p) = [("path", p)]
  toStringPairs (RelativePath p) = [("relativePath", p)]

data GitLocalSpec = GitLocalSpec {
  glPath :: LocalPath,
  glRef :: Maybe Template
} deriving (Show, Eq)

instance ToStringPairs GitLocalSpec where
  toStringPairs GitLocalSpec { glPath, glRef } =
    (toStringPairs glPath) <> optList (refAttr <$> glRef) where
      refAttr ref = ("ref", asString ref)

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

-- TODO return (SourceSpec, remainingAttrs)?
parseSourceSpecObject :: Value -> Object -> Parser SourceSpec
parseSourceSpecObject fetcher attrs = parseFetcher fetcher >>= parseSpec
  where
    parseFetcher :: Value -> Parser FetchType
    parseFetcher json = parseJSON json >>= bifoldMap invalid pure . parseFetchType
    parseSpec :: FetchType -> Parser SourceSpec
    parseSpec fetcher = case fetcher of
      FetchGithub ->
        build <$> owner <*> repo <*> refRequired where
          build ghOwner  ghRepo  ghRef = Github $ GithubSpec {
                ghOwner, ghRepo, ghRef }
      FetchGit -> build <$> url <*> refRequired where
        build gitUrl gitRef = Git $ GitSpec { gitUrl, gitRef }
      FetchGitLocal ->
        build <$> path <*> refOpt where
          build glPath glRef = GitLocal $ GitLocalSpec { glPath, glRef }
      FetchPath -> Path <$> path
      (FetchUrl t) -> buildUrl t <$> url
    owner = attrs .: "owner"
    repo = attrs .: "repo"
    url = attrs .: "url"
    path = (FullPath <$> attrs .: "path") <|> (RelativePath <$> attrs .: "relativePath")
    refRequired :: Parser Template = attrs .: "ref"
    refOpt :: Parser (Maybe Template) = attrs .:? "ref"
    buildUrl urlType url = Url $ UrlSpec { urlType, url = Template url }
    invalid v = fail $ "Unable to parse SourceSpec from: " <> (encodePrettyString v)

stringMapOfJson :: Aeson.Object -> Parser (HMap.HashMap String String)
stringMapOfJson args = HMap.fromList <$> (mapM convertArg $ HMap.toList args)
  where
    convertArg :: (T.Text, Aeson.Value) -> Parser (String, String)
    convertArg (k, v) =
      (\v -> (T.unpack k, T.unpack v)) <$> parseJSON v
  
data PackageSpec = PackageSpec {
  sourceSpec :: SourceSpec,
  fetchAttrs :: StringMap,
  packageAttrs :: StringMap
} deriving (Show, Eq)

instance FromJSON PackageSpec where
  parseJSON = withObject "PackageSpec" $ \attrs -> do
    (fetchJSON, attrs) <- attrs `extract` fetchKeyJSON
    (fetcher, attrs) <- attrs `extract` typeKeyJSON
    (fetchAttrs :: StringMap) <- parseJSON fetchJSON
    build
      <$> (parseSourceSpecObject fetcher attrs)
      <*> (pure fetchAttrs) <*> stringMapOfJson attrs
    where
      extract obj key = pairWithout key obj <$> obj .: key
      -- extractMaybe obj key = pairWithout key obj <$> obj .:? key
      pairWithout key obj v = (v, HMap.delete key obj)
      build sourceSpec fetchAttrs packageAttrs = PackageSpec {
        sourceSpec, fetchAttrs, packageAttrs
      }

instance ToJSON PackageSpec where
  toJSON PackageSpec { sourceSpec, fetchAttrs, packageAttrs } =
    toJSON
      . HMap.insert (T.unpack typeKeyJSON) (toJSON . fetcherNameWrangle . fetchType $ sourceSpec)
      . HMap.insert (T.unpack fetchKeyJSON) (toJSON fetchAttrs)
      $ (HMap.map toJSON (packageAttrs <> HMap.fromList (toStringPairs sourceSpec)))

newtype Packages = Packages
  { unPackages :: HMap.HashMap PackageName PackageSpec }
  deriving newtype (Show)

emptyPackages = Packages HMap.empty

add :: Packages -> PackageName -> PackageSpec -> Packages
add s name spec = Packages $ HMap.insert name spec $ unPackages s

remove :: Packages -> PackageName -> Packages
remove s name = Packages $ HMap.delete name $ unPackages s

emptyAttrs :: StringMap
emptyAttrs = HMap.empty

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
encodeFile path json = writeFileLazyBytestring path (encodePretty json)

writeFileLazyBytestring :: FilePath -> L.ByteString -> IO ()
writeFileLazyBytestring = writeFileContents' L.writeFile

writeFileText :: FilePath -> T.Text -> IO ()
writeFileText path text = writeFileContents' (\path -> B.writeFile path . E.encodeUtf8) path text

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
