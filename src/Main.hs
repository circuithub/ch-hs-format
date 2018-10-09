{-# language ApplicativeDo #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Data.Foldable ( for_ )
import Control.Monad ( (>=>) )
import Data.Data
import Language.Haskell.GHC.ExactPrint
import qualified Options.Applicative as OptParse

import CircuitHub.HsFormat
import CircuitHub.HsFormat.ModulePragmas
import CircuitHub.HsFormat.ModuleDeclaration

newtype Arguments =
  Arguments
    { inputFilePaths :: [ FilePath ]
    }


argumentsParser :: OptParse.Parser Arguments
argumentsParser = do
  inputFilePaths <-
    some
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
mainWith Arguments{ inputFilePaths } = for_ inputFilePaths $ \inputFilePath -> do
  ( anns, parsedSource ) <-
    parseModule inputFilePath
      >>= either ( fail . show ) return

  let
    ( formatted, ( anns', _ ), logEntries ) =
      runTransform anns ( formatTopDown parsedSource )

  mapM_ putStrLn logEntries

  writeFile inputFilePath ( exactPrint formatted anns' )




formatTopDown :: forall a. Data a => Formatter a
formatTopDown =
  tryAndFormat formatLanguagePragmas
    >=> tryAndFormat moduleDecl
    >=>
      -- Continue formatting children
      gmapM formatTopDown
