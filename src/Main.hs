{-# language ApplicativeDo #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Data.Foldable ( for_ )
import Control.Monad ( (>=>), when )
import Data.Data
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers ( parseModuleFromString )
import qualified Options.Applicative as OptParse
import qualified Data.Text as T
import qualified Data.Text.IO as T

import CircuitHub.HsFormat
import CircuitHub.HsFormat.IE
import CircuitHub.HsFormat.Import
import CircuitHub.HsFormat.ModulePragmas
import CircuitHub.HsFormat.ModuleDeclaration
import CircuitHub.HsFormat.TopLevelDeclarations


newtype Arguments =
  Arguments
    { inputFilePaths :: [ FilePath ]
    }


argumentsParser :: OptParse.Parser Arguments
argumentsParser = do
  inputFilePaths <-
    many
      ( OptParse.strArgument
          ( mconcat
              [ OptParse.metavar "FILE"
              , OptParse.help "The file to format"
              ]
          )
      )

  return Arguments{..}


main :: IO ()
main =
  OptParse.execParser ( OptParse.info argumentsParser mempty )
    >>= mainWith


mainWith :: Arguments -> IO ()
mainWith Arguments{ inputFilePaths = [] } =  do
  contents <- getContents

  ( anns, parsedSource ) <-
    parseModuleFromString "stdin" contents
      >>= either ( \_ -> fail "Could not parse" ) return

  let
    ( formatted, ( anns', _ ), logEntries ) =
      runTransform anns ( formatTopDown parsedSource )

  mapM_ putStrLn logEntries

  putStr ( exactPrint formatted anns' )

mainWith Arguments{ inputFilePaths } = for_ inputFilePaths $ \inputFilePath -> do
  parseModule inputFilePath >>= \case
    Left errors -> do
      putStrLn $ "Failed to parse " <> inputFilePath <> ":"
      for_ errors $ \e ->
        putStrLn $ "  - " <> show e

    Right ( anns, parsedSource ) -> do
      let
        ( formatted, ( anns', _ ), logEntries ) =
          runTransform anns ( formatTopDown parsedSource )

      mapM_ putStrLn logEntries

      oldSrc <- T.unpack <$> T.readFile inputFilePath
      let newSrc = exactPrint formatted anns'

      when (oldSrc /= newSrc) $
        writeFile inputFilePath newSrc


formatTopDown :: forall a. Data a => Formatter a
formatTopDown =
  tryAndFormat formatLanguagePragmas
    >=> tryAndFormat moduleDecl
    >=> tryAndFormat formatImport
    >=> tryAndFormat formatIE
    >=> tryAndFormat topLevelDeclarations
    >=>
      -- Continue formatting children
      gmapM formatTopDown
