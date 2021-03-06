{-| This module contains the top level and options parsing of the @dhall-docs@
    executable
-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Docs
    ( -- * Options
      Options(..)
    , GenerationStrategy(..)
    , parserInfoOptions
    , parseOptions

      -- * Execution
    , main
    , defaultMain
    , getAllDhallFiles

      -- * Miscelaneous
    , saveHtml
    , createIndexes
    ) where

import Data.Monoid         ((<>))
import Dhall.Docs.Embedded
import Dhall.Docs.Html
import Dhall.Parser        (Header, exprAndHeaderFromText)
import Options.Applicative (Parser, ParserInfo)
import Path                (Abs, Dir, File, Path, (</>))

import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe
import qualified Data.Text.Encoding  as Text.Encoding
import qualified Lucid
import qualified Options.Applicative
import qualified Path
import qualified Path.IO

{-| To specify if the tool should generate a single HTML page with all the
    package information or one for each file in your package
-}
data GenerationStrategy
    = SinglePage
    | MultiPage
    deriving Show

-- | Command line options
data Options = Options
    { packageDir :: FilePath         -- ^ Directory where your package resides
    , strategy :: GenerationStrategy -- ^ Output strategy of the tool
    , outDir :: FilePath             -- ^ Directory where your documentation
                                     --   will be placed
    }
    deriving Show

parseStrategy :: Parser GenerationStrategy
parseStrategy =
    Options.Applicative.flag
        MultiPage
        SinglePage
        (   Options.Applicative.long "single-page"
        <>  Options.Applicative.help
                "Generate a single page HTML documentation. By default, the tool will generate \
                \a multi-page documentation"
        )

-- | `Parser` for the `Options` type
parseOptions :: Parser Options
parseOptions =
        Options
    <$> Options.Applicative.strOption
        ( Options.Applicative.long "input"
       <> Options.Applicative.metavar "INPUT"
       <> Options.Applicative.help "Directory of your dhall package" )
    <*> parseStrategy
    <*> Options.Applicative.strOption
        ( Options.Applicative.long "output"
       <> Options.Applicative.metavar "OUTPUT"
       <> Options.Applicative.help "Directory where your docs will be generated"
       <> Options.Applicative.value "docs" )

-- | `ParserInfo` for the `Options` type
parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    let progDesc = "Generate HTML documentation from a dhall package or file" in
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.progDesc progDesc
        )

{-| Fetches a list of all dhall files in a directory. This is not the same
    as finding all files that ends in @.dhall@, but finds all files that
    successfully parses as a valid dhall file.

    The reason it doesn't guide the search by its extension is because of the
    dhall <https://prelude.dhall-lang.org Prelude>
    That package doesn't ends any of their files in @.dhall@.
-}
getAllDhallFiles
    :: Path Abs Dir -- ^ Base directory to do the search
    -> IO [(Path Abs File, Header)]
getAllDhallFiles baseDir = do
    files <- snd <$> Path.IO.listDirRecur baseDir
    Data.Maybe.catMaybes <$> mapM readDhall files
  where
    readDhall :: Path Abs File -> IO (Maybe (Path Abs File, Header))
    readDhall absFile = do
        let filePath = Path.fromAbsFile absFile
        fileContents <- Data.ByteString.readFile filePath
        return $ case Text.Encoding.decodeUtf8' fileContents of
            Left _ -> Nothing
            Right contents ->
                case exprAndHeaderFromText filePath contents of
                    Right (header, _) -> return (absFile, header)
                    _ -> Nothing


{-| Calculate the relative path needed to access files on the first argument
    relative from the second argument.

    The second argument needs to be a child of the first, otherwise it will
    loop forever

    Examples:

    >>> resolveRelativePath $(mkAbsDir "/a/b/c/") $(mkAbsDir "/a/b/c/d/e/")
    "../../"
    >>> resolveRelativePath $(mkAbsDir "/a/") $(mkAbsDir "/a/")
    ""
-}
resolveRelativePath :: Path Abs Dir -> Path Abs Dir -> FilePath
resolveRelativePath outDir currentDir =
    if outDir == currentDir then ""
    else "../" <> resolveRelativePath outDir (Path.parent currentDir)

{-| Saves the HTML file from the input package to the output destination
-}
saveHtml
    :: Path Abs Dir             -- ^ Input package directory.
                                --   Used to remove the prefix from all other dhall
                                --   files in the package
    -> Path Abs Dir             -- ^ Output directory
    -> (Path Abs File, Header)  -- ^ (Input file, Parsed header)
    -> IO (Path Abs File)       -- ^ Output path file
saveHtml inputAbsDir outputAbsDir t@(absFile, _) = do
    htmlOutputFile <- (outputAbsDir </>)
            <$> (Path.stripProperPrefix inputAbsDir absFile
                >>= addHtmlExt)

    let htmlOutputDir = Path.parent htmlOutputFile

    Path.IO.ensureDir htmlOutputDir

    let relativeResources = resolveRelativePath outputAbsDir htmlOutputDir

    Lucid.renderToFile (Path.fromAbsFile htmlOutputFile)
        $ filePathHeaderToHtml t (relativeResources <> "index.css")
    return htmlOutputFile
  where
    addHtmlExt :: Path b File -> IO (Path b File)
#if MIN_VERSION_path(0,7,0)
    addHtmlExt = Path.addExtension ".html"
#else
    addHtmlExt = Path.addFileExtension "html"
#endif

{-| Create an index.html file on each folder available in the second argument
    that lists all the contents on that folder.

    Examples:

    >>> createIndexes $(mkAbsDir "/") [
        , $(mkAbsFile "/a/b.txt")
        , $(mkAbsFile "/a/c/b.txt")
        , $(mkAbsFile "/a/c.txt")
        ]

    ... will create two index.html files:

    1. $(mkAbsFile "/a/index.html"), that will list the @"/a/b.txt"@ and
    @"/a/c.txt"@ files
    2. $(mkAbsFile "/a/c/index.html") that will list the @"a/c/b.txt"@ file

-}
createIndexes
    :: Path Abs Dir    -- ^ Directory where index.html file will be saved. Used
                       --   to link the css resources. It should be a prefix for
                       --   each @Path Abs File@ on the second argument
    -> [Path Abs File] -- ^ Html files generated by the tool
    -> IO ()
createIndexes outputPath htmlFiles = do
    let toMap file = Map.singleton (Path.parent file) [file]
    let filesGroupedByDir = Map.unionsWith (<>) $ map toMap htmlFiles

    let createIndex index files = do
            indexFile <- Path.fromAbsFile . (index </>) <$> Path.parseRelFile "index.html"
            let relativeResources = resolveRelativePath outputPath index
            Lucid.renderToFile indexFile $
                indexToHtml
                    (Path.fromAbsDir index)
                    (map Path.fromAbsFile files)
                    (relativeResources <> "index.css")

    _ <- Map.traverseWithKey createIndex filesGroupedByDir
    return ()

-- | Default execution of @dhall-docs@ command
defaultMain :: Options -> IO ()
defaultMain Options{..} = do
    resolvedPackageDir <- Path.IO.resolveDir' packageDir
    resolvedOutDir <- Path.IO.resolveDir' outDir
    dhallFiles <- getAllDhallFiles resolvedPackageDir
    generatedHtmlFiles <-
        mapM (saveHtml resolvedPackageDir resolvedOutDir) dhallFiles
    createIndexes resolvedOutDir generatedHtmlFiles

    dataDir <- getDataDir
    Control.Monad.forM_ dataDir $ \(filename, contents) -> do
        let finalPath = Path.fromAbsFile $ resolvedOutDir </> filename
        Data.ByteString.writeFile finalPath contents

-- | Entry point for the @dhall-docs@ executable
main :: IO ()
main = Options.Applicative.execParser parserInfoOptions >>= defaultMain
