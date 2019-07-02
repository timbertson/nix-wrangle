{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Wrangle.Cmd where

import Prelude hiding (error)
import Control.Applicative
import Control.Monad
import Control.Monad.Except (throwError)
-- import Control.Exception (throw, AssertionFailed(..))
-- import qualified Data.HashMap.Strict as HMap
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Wrangle.Source (PackageName(..))
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
-- import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as LB
-- import qualified Nix.Expr as N
import Wrangle.Util
import qualified Wrangle.Fetch as Fetch
import qualified Wrangle.Source as Source
import qualified System.Directory as Dir
import qualified Wrangle.Splice as Splice
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
import qualified System.FilePath.Posix as PosixPath

main :: IO ()
main = join $ Opts.execParser opts
  where
    opts = Opts.info (parseCommand <**> Opts.helper) $ mconcat desc
    desc =
      [ Opts.fullDesc
      , Opts.header "Nix-wrangle - source & dependency manager for Nix projects"
      ]

parseCommand :: Opts.Parser (IO ())
parseCommand = Opts.subparser (
    Opts.command "init" parseCmdInit <>
    Opts.command "add" parseCmdAdd <>
    Opts.command "update" parseCmdUpdate <>
    Opts.command "splice" parseCmdSplice <>
    Opts.command "show" parseCmdShow <>
    Opts.command "default-nix" parseCmdDefaultNix
    )

{-

Planned commands / terminology:

nix/default.nix: derivation base. Contains deps & build instructions but not src
default.nix: concrete derivation: bakes in version of nixpkgs, self & wrangle
nix/wrangle.json: public deps
nix/wrangle-local.json: local deps

TODO:
- allow local overlays on global sources
  e.g. prefetch against a local git repo but publish with the public URL
- splice: specify output path

Use cases:
 - nix-wrangle splice: splice `self` into derivation base, to be used upstream (i.e. in nixpkgs)
 - nix-wrangle init: generate initial sources/wrangle.nix with wrangle and an optional pinned nixpkgs
 - nix-wrangle default-nix: generate default.nix
 - nix-wrangle show: dump current details
 - nix-wrangle show --update: compare current versions against "head" / latest releases
 - nix-wrangle rm name [--source local]
 - nix-wrangle add name --type github [--source public]
 - nix-wrangle update name [--source public] (if no args given, does an auto-update)

 -}

newtype CommonOpts = CommonOpts {
  sources :: Maybe (NonEmpty Source.SourceFile)
} deriving newtype Show

parseCommon :: Opts.Parser CommonOpts
parseCommon =
  build <$> parseSources <*> parseLocal <*> parsePublic
  where
    build src a b = CommonOpts { sources = NonEmpty.nonEmpty (src <> a <> b) }
    parseSources = many $ Source.NamedSource <$> Opts.strOption
      ( Opts.long "source" <>
        Opts.short 's' <>
        Opts.metavar "SOURCE.json" <>
        Opts.help "Specify wrangle.json file to operate on"
      )
    parseLocal = Opts.flag [] [Source.LocalSource]
      ( Opts.long "local" <>
        Opts.help "use nix/wrangle-local.json"
      )
    parsePublic = Opts.flag [] [Source.DefaultSource]
      ( Opts.long "public" <>
        Opts.help "use nix/wrangle.json"
      )

parseName :: Opts.Parser Source.PackageName
parseName = Source.PackageName <$> Opts.argument Opts.str (Opts.metavar "NAME")

parseNames :: Opts.Parser [Source.PackageName]
parseNames = many parseName

parseAdd :: Opts.Parser (Either AppError (PackageName, Source.PackageSpec))
parseAdd =
  mapLeft AppError <$> (build <$> Opts.optional parseName <*> parsePackageAttrs)
  where
    build :: (Maybe PackageName) -> (Source.StringMap) -> Either String (PackageName, Source.PackageSpec)
    build name attrs =
      case ((Source.parseFetchType <$> lookup "type") `orElse` Right Source.FetchGithub)  of
        Right Source.FetchGithub -> buildGithub
        Right (Source.FetchUrl urlType) -> buildUrl urlType
        Right Source.FetchPath -> buildLocalPath
        Right Source.FetchGitLocal -> buildGitLocal
        Right Source.FetchGit -> buildGit
        Left err -> throwError err
      where
        packageSpec :: Source.SourceSpec -> Source.PackageSpec
        packageSpec sourceSpec = Source.PackageSpec {
          Source.sourceSpec,
          -- TODO drop consumed attrs? Duplicates shouldn't matter
          -- as long as SourceSpec doesn't transform inputs.
          Source.packageAttrs = attrs,
          Source.fetchAttrs = Source.emptyAttrs
        }

        lookup attr = HMap.lookup attr attrs

        require :: String -> Either String String
        require k = toRight ("--"<>k<>" required") (lookup k)

        requireName :: Either String PackageName
        requireName = toRight "--name required" name

        buildPath :: Either String Source.LocalPath
        buildPath = build <$> require "path" where
          build p = if PosixPath.isAbsolute p then Source.FullPath p else Source.RelativePath p

        buildLocalPath :: Either String (PackageName, Source.PackageSpec)
        buildLocalPath = build <$> requireName <*> buildPath where
          build name path = (name, packageSpec $ Source.Path path)

        buildGit :: Either String (PackageName, Source.PackageSpec)
        buildGit = build <$> requireName <*> require "url" <*> (Source.Template <$> require "ref") where
          build name gitUrl gitRef = (name,
            packageSpec $ Source.Git $ Source.GitSpec { Source.gitUrl, Source.gitRef })

        buildGitLocal :: Either String (PackageName, Source.PackageSpec)
        buildGitLocal = build <$> requireName <*> buildPath <*> (Source.Template <$> require "ref") where
          build name glPath glRef = (name,
            packageSpec $ Source.GitLocal $ Source.GitLocalSpec { Source.glPath, Source.glRef })

        buildUrl :: Source.UrlFetchType -> Either String (PackageName, Source.PackageSpec)
        buildUrl urlType = build <$> requireName <*> require "url" where
          build name url = (name, packageSpec $ Source.Url Source.UrlSpec {
            Source.urlType = urlType,
            Source.url = Source.Template url
          })

        buildGithub :: Either String (PackageName, Source.PackageSpec)
        buildGithub = build <$> identity
          where
            build (name, ghOwner, ghRepo) =
              (name, packageSpec $ Source.Github Source.GithubSpec { Source.ghOwner, Source.ghRepo, Source.ghRef })
              where ghRef = (fromMaybe (Source.Template "master") ref)

            ref = Source.Template <$> lookup "ref"

            identity :: Either String (PackageName, String, String)
            identity = case (name, lookup "owner", lookup "repo") of
              (name, Just owner, Just repo) -> Right (fromMaybe (PackageName repo) name, owner, repo)
              -- (Just name, Just owner, Nothing) -> Right (PackageName name, owner, name)
              (Just (PackageName name), Nothing, Nothing) -> case span (/= '/') name of
                (owner, '/':repo) -> Right (PackageName repo, owner, repo)
                _ -> throwError ("`" <> name <> "` doesn't look like a github repo")
              (Nothing, _, _) -> throwError "name or --owner/--repo required"
              (_, Nothing, Just _) -> throwError "--owner required when using --repo"
              (_, Just _, Nothing) -> throwError "--repo required when using --owner"

parsePackageAttrs :: Opts.Parser (Source.StringMap)
parsePackageAttrs = HMap.fromList <$> many parseAttribute where
  parseAttribute :: Opts.Parser (String, String)
  parseAttribute =
    Opts.option (Opts.maybeReader parseKeyVal)
      ( Opts.long "attr" <>
        Opts.short 'a' <>
        Opts.metavar "KEY=VAL" <>
        Opts.help "Set the package spec attribute <KEY> to <VAL>"
      ) <|> shortcutAttributes <|>
    (("type",) <$> Opts.strOption
      ( Opts.long "type" <>
        Opts.short 't' <>
        Opts.metavar "TYPE" <>
        Opts.help ("The source type. "<> Source.validTypesDoc)
      ))

  -- Parse "key=val" into ("key", "val")
  parseKeyVal :: String -> Maybe (String, String)
  parseKeyVal str = case span (/= '=') str of
    (key, '=':val) -> Just (key, val)
    _ -> Nothing

  -- Shortcuts for known attributes
  shortcutAttributes :: Opts.Parser (String, String)
  shortcutAttributes = foldr (<|>) empty $ mkShortcutAttribute <$>
    [
      ("ref", "github / git / git-local"),
      ("owner", "github"),
      ("repo", "github"),
      ("url", "url / file / git"),
      ("path", "git-local"),
      ("version", "all")
    ]

  mkShortcutAttribute :: (String, String) -> Opts.Parser (String, String)
  mkShortcutAttribute (attr, types) =
    (attr,) <$> Opts.strOption
      ( Opts.long attr <>
        Opts.metavar (toUpper <$> attr) <>
        Opts.help
          (
            "Equivalent to --attr " <> attr <> "=" <> (toUpper <$> attr) <>
            ", used for source type " <> types
          )
      )

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------
parseCmdShow :: Opts.ParserInfo (IO ())
parseCmdShow =
  Opts.info
    ((cmdShow <$> parseCommon) <**>
      Opts.helper) $
    mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Show sources"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle info"
      ]

cmdShow :: CommonOpts -> IO ()
cmdShow opts =
  do
    sourceFiles <- requireConfiguredSources $ sources opts
    sources <- Source.loadSources sourceFiles
    putStrLn $ Source.encodePrettyString sources

requireConfiguredSources :: Maybe (NonEmpty Source.SourceFile) -> IO (NonEmpty Source.SourceFile)
requireConfiguredSources sources =
  Source.configuredSources sources >>=
    (liftMaybe (AppError "No wrangle JSON files found"))

-------------------------------------------------------------------------------
-- Init
-------------------------------------------------------------------------------
parseCmdInit :: Opts.ParserInfo (IO ())
parseCmdInit =
  Opts.info
    (pure cmdInit <**>
      Opts.helper) $
    mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Initialize nix-wrangle"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle init"
      ]

cmdInit :: IO ()
cmdInit = do
  cmdAdd (Right (PackageName "nix-wrangle", spec)) commonOpts
  updateDefaultNix defaultNixOptsDefault
  where
    commonOpts = CommonOpts { sources = Nothing }
    spec = Source.PackageSpec {
      Source.sourceSpec = Source.Github Source.GithubSpec {
        Source.ghOwner = "timbertson",
        Source.ghRepo = "nix-wrangle",
        Source.ghRef = Source.Template "v1"
      },
      Source.fetchAttrs = HMap.empty,
      Source.packageAttrs = HMap.empty
    }

-------------------------------------------------------------------------------
-- Add
-------------------------------------------------------------------------------
parseCmdAdd :: Opts.ParserInfo (IO ())
parseCmdAdd =
  Opts.info
    ((cmdAdd <$> parseAdd <*> parseCommon) <**>
      Opts.helper) $
    mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Add sources"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle add"
      ]

cmdAdd :: Either AppError (PackageName, Source.PackageSpec) -> CommonOpts -> IO ()
cmdAdd addOpt opts =
  do
    (name, inputSpec) <- liftEither addOpt
    putStrLn $ "Adding " <> show name <> " // " <> show inputSpec
    configuredSources <- Source.configuredSources $ sources opts
    let extantSourceFile = NonEmpty.head <$> configuredSources
    debugLn $ "extantSourceFile: " <> show extantSourceFile
    let loadedSourceFile = loadSource' <$> extantSourceFile
    source :: (Source.SourceFile, Maybe Source.Packages) <- fromMaybe (return (Source.DefaultSource, Nothing)) loadedSourceFile
    debugLn $ "source: " <> show source
    let (sourceFile, inputSource) = source
    let baseSource = fromMaybe (Source.emptyPackages) inputSource
    spec <- Fetch.prefetch name inputSpec
    let modifiedSource = Source.add baseSource name spec
    Source.writeSourceFile sourceFile modifiedSource
  where
    loadSource' :: Source.SourceFile -> IO (Source.SourceFile, Maybe Source.Packages)
    -- TODO: arrows?
    loadSource' f = (\(a,b) -> (a, Just b)) <$> loadSource f

loadSource :: Source.SourceFile -> IO (Source.SourceFile, Source.Packages)
loadSource f = (,) f <$> Source.loadSourceFile f
      
-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------
parseCmdUpdate :: Opts.ParserInfo (IO ())
parseCmdUpdate =
  Opts.info
    ((cmdUpdate <$> Opts.optional parseNames <*> parsePackageAttrs <*> parseCommon) <**> Opts.helper) $
    mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Update sources"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle update"
      ]

cmdUpdate :: Maybe [PackageName] -> Source.StringMap -> CommonOpts -> IO ()
cmdUpdate packageNamesOpt updateAttrs opts = do
  sourceFiles <- requireConfiguredSources $ sources opts
  sources <- sequence $ loadSource <$> sourceFiles
  checkMissingKeys (snd <$> sources)
  sequence_ $ updateSources <$> sources
  where
    checkMissingKeys :: NonEmpty Source.Packages -> IO ()
    checkMissingKeys sources = case missingKeys of
      [] -> return ()
      _ -> fail $ "No such packages: " <> show missingKeys
      where
        (_, missingKeys) = partitionPackageNames $ Source.merge sources

    partitionPackageNames :: Source.Packages -> ([PackageName], [PackageName])
    partitionPackageNames sources = case packageNamesOpt of
      Nothing -> (Source.keys sources, [])
      (Just names) -> partition (Source.member sources) names
    
    updateSingle :: Source.Packages -> PackageName -> IO Source.Packages
    updateSingle packages name = do
      infoLn $ " - updating " <> (show name) <> "..."
      original <- liftEither $ Source.lookup name packages
      newSpec <- liftEither $ Source.updatePackageSpec original updateAttrs
      fetched <- if newSpec /= original
        then Fetch.prefetch name newSpec
        else (infoLn "   ... unchanged" >> return newSpec)
      return $ Source.add packages name fetched

    updateSources :: (Source.SourceFile, Source.Packages) -> IO ()
    updateSources (sourceFile, sources) = do
      infoLn $ "Updating "<> Source.pathOfSource sourceFile <> " ..."
      let (packageNames, _) = partitionPackageNames sources
      updated <- foldM updateSingle sources packageNames
      Source.writeSourceFile sourceFile updated

-------------------------------------------------------------------------------
-- Splice
-------------------------------------------------------------------------------
parseCmdSplice :: Opts.ParserInfo (IO ())
parseCmdSplice =
  Opts.info
    ((cmdSplice <$> parseCommon <*> parseNixPath) <**>
      Opts.helper) $
    mconcat desc
  where
    parseNixPath = Opts.argument Opts.str (Opts.metavar "NIX_FILE")
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Splice `self` dependency into a derivation base (typically nix/default.nix)"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle Splice"
      ]

cmdSplice :: CommonOpts -> FilePath -> IO ()
cmdSplice opts path =
  do
    fileContents <- Splice.load path
    let expr = Splice.parse fileContents
    expr <- Splice.getExn expr
    -- putStrLn $ show $ expr
    sourceFiles <- requireConfiguredSources $ sources opts
    sources <- Source.merge <$> Source.loadSources sourceFiles
    self <- liftEither $ Source.lookup (PackageName "self") sources
    let existingSrcSpans = Splice.extractSourceLocs expr
    srcSpan <- case existingSrcSpans of
      [single] -> return single
      other -> fail $ "No single source found in " ++ (show other)
    replacedText <- liftEither $ Splice.replaceSourceLoc fileContents self srcSpan
    putStrLn (T.unpack replacedText)


-------------------------------------------------------------------------------
-- default-nix
-------------------------------------------------------------------------------
parseCmdDefaultNix :: Opts.ParserInfo (IO ())
parseCmdDefaultNix =
  Opts.info
    ((pure cmdDefaultNix) <**>
      Opts.helper) $
    mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Show sources"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle info"
      ]

cmdDefaultNix :: IO ()
cmdDefaultNix = updateDefaultNix (DefaultNixOpts { force = True })

data DefaultNixOpts = DefaultNixOpts {
  force :: Bool
}
defaultNixOptsDefault = DefaultNixOpts { force = False }

updateDefaultNix :: DefaultNixOpts -> IO ()
updateDefaultNix (DefaultNixOpts { force }) = do
  continue <- if force then return True else shouldWriteFile
  if continue then writeFile
  else infoLn $ "Note: not replacing existing "<>path<>", run `nix-wrangle default-nix` to explicitly override"
  where
    path = "default.nix"
    markerText :: T.Text = "# Note: This file is autogenerated by nix-wrangle"
    contents :: T.Text
    contents = T.unlines [
      markerText,
      "# It can be regenerated with `nix-wrangle default-nix`",
      -- minimal implementation of fetching for nixpkgs + wrangle. Only supports github/url from public JSON file
      "let",
      "  systemNixpkgs = import <nixpkgs> {};",
      "  fetchers = pkgs: {",
      "    github = pkgs.fetchFromGitHub;",
      "    url = builtins.fetchTarball;",
      "  };",
      "  fetch = pkgs: source: ",
      "    (builtins.getAttr source.type fetchers) source.fetch;",
      "  sourcesJson = (builtins.fromJSON (builtins.readFile ./nix/wrangle.json)).sources;",
      "  defaultPkgs = if builtins.hasAttr \"pkgs\" sourcesJson",
      "    then fetch systemNixpkgs sourcesJson.pkgs else systemNixpkgs;",
      "  defaultWrangle = pkgs: let wrangle = sourcesJson.nix-wrangle or abort \"No nix-wrangle entry in nix/wrangle.json\"; in",
      "    { api = args: pkgs.callPackage \"${fetch pkgs wrangle}/${wrangle.nix}\" args; };",
      "in",
      "{ pkgs ? defaultPkgs, nix-wrangle ? defaultWrangle pkgs }:",
      "(nix-wrangle.api { inherit pkgs; }).inject ./nix" ]

    writeFile :: IO ()
    writeFile = do
      infoLn $ "Updating " <> path
      Source.writeFileContents path (LB.fromStrict . TE.encodeUtf8 $ contents)

    shouldWriteFile :: IO Bool
    shouldWriteFile = do
      exists <- Dir.doesFileExist path
      if exists then
        (T.isInfixOf markerText) <$> TE.decodeUtf8 <$> B.readFile path
      else
        return True

