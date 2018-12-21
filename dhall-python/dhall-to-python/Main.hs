{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Dhall.Python (Conversion)
import Options.Applicative (Parser, ParserInfo)

import qualified Language.Python.Common.Pretty as Py
import Language.Python.Common.PrettyAST()

import qualified Control.Exception
import qualified Data.ByteString.Char8
import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.Python
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified Paths_dhall_python as Meta
import qualified System.Exit
import qualified System.IO

data Options = Options
    { explain    :: Bool
    , pretty     :: Bool
    , omitNull   :: Bool
    , version    :: Bool
    , conversion :: Conversion
    }

parseOptions :: Parser Options
parseOptions = Options.Applicative.helper <*> do
    explain    <- parseExplain
    pretty     <- parsePretty
    omitNull   <- parseOmitNull
    version    <- parseVersion
    conversion <- Dhall.Python.parseConversion
    return (Options {..})
  where
    parseExplain =
        Options.Applicative.switch
            (   Options.Applicative.long "explain"
            <>  Options.Applicative.help "Explain error messages in detail"
            )

    parsePretty =
        prettyFlag <|> compactFlag <|> defaultBehavior
      where
        prettyFlag =
            Options.Applicative.flag'
                True
                (   Options.Applicative.long "pretty"
                <>  Options.Applicative.help "Pretty print generated Python"
                )

        compactFlag =
            Options.Applicative.flag'
                False
                (   Options.Applicative.long "compact"
                <>  Options.Applicative.help "Render Python on one line"
                )

        defaultBehavior =
            pure False

    parseOmitNull =
        Options.Applicative.switch
            (   Options.Applicative.long "omitNull"
            <>  Options.Applicative.help "Omit record fields that are null"
            )

    parseVersion =
        Options.Applicative.switch
            (   Options.Applicative.long "version"
            <>  Options.Applicative.help "Display version"
            )

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        parseOptions
        (   Options.Applicative.fullDesc
        <>  Options.Applicative.progDesc "Compile Dhall to Python"
        )

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Options {..} <- Options.Applicative.execParser parserInfo

    when version $ do
      putStrLn (showVersion Meta.version)
      System.Exit.exitSuccess

    handle $ do
        let encode = Py.prettyText

        let explaining = if explain then Dhall.detailed else id

        -- let omittingNull = if omitNull then Dhall.Python.omitNull else id
        let omittingNull = id

        stdin <- Data.Text.IO.getContents

        python <- omittingNull <$> explaining (Dhall.Python.codeToValue conversion "(stdin)" stdin)

        Data.ByteString.Char8.putStrLn $ Data.ByteString.Char8.pack $ encode python

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
