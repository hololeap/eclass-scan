{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Gentoo.Eclass
    ( RepoName
    , RepoPathBS
    , Repository(..)
    , Eclass(..)
    , EclassVar(..)
    , scanRepos
    ) where

import Conduit
import Control.Applicative
import Control.Exception.Safe (throwString)
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec
import Data.Conduit.Lazy (lazyConsume)
import Data.Conduit.Process (sourceCmdWithStreams)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory
import System.Exit
import System.FilePath

import Prelude hiding (takeWhile)

type RepoName = ByteString

-- Initial ByteString path to be converted to FilePath later on
type RepoPathBS = ByteString
type RepoPath = FilePath

data Repository = Repository
    { repositoryName :: RepoName
    , repositoryLocation :: RepoPath
    , repositoryEclasses :: [Eclass]
    } deriving (Show, Eq, Ord)

data Eclass = Eclass
    { eclassName :: String
    , eclassVars :: [EclassVar]
    } deriving (Show, Eq, Ord)

newtype EclassVar = EclassVar ByteString
    deriving (Show, Eq, Ord)

-- | Run @portageq@ to gather a list of repo names and paths, then scan each
--   one for eclasses and ultimately eclass metadata.
scanRepos :: IO [Repository]
scanRepos = do
    let cmd = "/usr/bin/portageq repos_config /" :: String
    nps <- runOrDie cmd
        $  conduitParser repoParser
           -- Remove the PositionRange component and filter out Nothings
        .| concatMapC snd
           -- Convert the RepoPathBS to RepoPath
        .| mapC (second (T.unpack . T.decodeUtf8))
        .| sinkList
    -- Stream the list of repo paths to 'getEclasses', map to 'Repository'
    lazyConsume
        $  yieldMany nps
        .| mapMC (\(n,l) -> Repository n l <$> getEclasses l)

-- | Get the name of the repo and its path from blocks outputted by
--   @portageq@. If the path doesn't exist, this will return @Nothing@.
repoParser :: Parser (Maybe (RepoName, RepoPathBS))
repoParser = do
    n <- repoName
    mp <- repoBlock
    pure $ (n,) <$> mp
  where
    -- Get the name of the repo at the top of the block
    repoName :: Parser RepoName
    repoName = do
        _ <- char '['
        n <- takeWhile (/= ']')
        _ <- char ']'
        _ <- endOfLine
        pure n

    -- Parse the block for location field
    repoBlock :: Parser (Maybe RepoPathBS)
    repoBlock = choice
        [ do
            l <- "location = " *> takeLine
            -- Found the location, skip the rest of the block
            skipMany miscLine *> endOfBlock
            pure $ Just l
          -- Did not find the location, keep trying
        , miscLine *> repoBlock
          -- Reached the end of the block, no location field
        , Nothing <$ endOfBlock
        ]

    miscLine :: Parser ()
    miscLine = skipNonEmptyLine

    -- A block either ends with an empty line or eof
    endOfBlock :: Parser ()
    endOfBlock = endOfLine <|> endOfInput

-- | Scan the repo path for @*.eclass@ files in @eclass/@, then run
--   'eclassParser' on each of them to produce @[Eclass]@.
--
--   If the @eclass/@ directory doesn't exist, the scan is skipped for that
--   repo.
getEclasses :: RepoPath -> IO [Eclass]
getEclasses repoLoc = do
    let eclassDir = repoLoc </> "eclass"
    -- Ignore errors due to non-existant eclass dir, returning an empty list
    option [] (listDirectory eclassDir) >>= \fs ->
        lazyConsume
             $ yieldMany fs
            .| mapC splitExtensions
            .| filterC (\(_,e) -> e == ".eclass")
            .| mapMC (mkEclass eclassDir)
  where
    mkEclass :: FilePath -> (FilePath, String) -> IO Eclass
    mkEclass eclassDir (n,e) = do
        vs <- parseFile eclassParser (eclassDir </> n <.> e)
        pure $ Eclass n vs

eclassParser :: Parser [EclassVar]
eclassParser = choice
        [ -- cons the EclassVar to the list and continue
          liftA2 (:) eclassVar eclassParser
          -- or skip the line and continue
        , skipLine *> eclassParser
          -- or end the list on eof
        , [] <$ endOfInput
        ]
  where
    -- Scans for @ECLASS_VARIABLE comments rather than parsing the raw bash
    eclassVar :: Parser EclassVar
    eclassVar = "# @ECLASS_VARIABLE: " *> (EclassVar <$> takeLine)

takeLine :: Parser ByteString
takeLine = A.takeWhile (not . isEndOfLine) <* endOfLine

skipNonEmptyLine :: Parser ()
skipNonEmptyLine = A.satisfy (not . isEndOfLine) *> skipLine

skipLine :: Parser ()
skipLine = A.skipWhile (not . isEndOfLine) <* endOfLine

parseFile :: Parser a -> FilePath -> IO a
parseFile p f = runConduitRes
    $ sourceFile f .| sinkParser p

-- | Run the command and pipe stdout to the given conduit.
--
--   * stdin is ignored
--   * stderr is piped to stderr in the console
--   * exit failures are thrown via throwString
runOrDie
    :: String -- ^ Command to run (including args)
    -> ConduitT ByteString Void IO a -- ^ Stream for stdout
    -> IO a
runOrDie cmd outC = do
    (ec, x, _) <- sourceCmdWithStreams cmd (pure ()) outC stderrC
    case ec of
        ExitSuccess -> pure x
        ExitFailure i -> throwString $ unwords
            [ show cmd, "failed with exit code", show i]
