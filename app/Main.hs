{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Main where

import           "base" Control.Monad                       (join, (<=<), (>=>))

import qualified "containers" Data.Map                      as Map
import           "containers" Data.Set                      (Set)
import qualified "containers" Data.Set                      as Set

import           "optparse-applicative" Options.Applicative (ParserInfo,
                                                             argument, command,
                                                             execParser,
                                                             fullDesc, helper,
                                                             info, metavar,
                                                             progDesc, str,
                                                             subparser, (<**>))

import           "HaTeX" Text.LaTeX.Base.Parser             (parseLaTeXFile)
import           "HaTeX" Text.LaTeX.Base.Render             (render)
import           "HaTeX" Text.LaTeX.Base.Syntax             (LaTeX (..),
                                                             TeXArg (..),
                                                             lookForCommand)

import           "text" Data.Text                           (Text)
import qualified "text" Data.Text                           as Text

import           "tex2nix" Tex.Tlpdb


data Args
    = DumpPackagesArgs
        { file :: FilePath
        }
    | IndexDbArgs
        { file :: FilePath
        }
  deriving (Eq, Ord, Show)

argsParser :: ParserInfo Args
argsParser = info (cmds <**> helper) fullDesc
  where
    cmds = subparser $ mconcat
        [ command "dump"  (info dump  $ progDesc "dump package names of tex file")
        , command "index" (info index $ progDesc "index texlive package database")
        ]
    dump  = DumpPackagesArgs <$> argument str (metavar "FILE")
    index = IndexDbArgs      <$> argument str (metavar "FILE")

main :: IO ()
main = execParser argsParser >>= \case
    DumpPackagesArgs{ file } -> dumpPkgs file
    IndexDbArgs{ file } -> indexDb file
  where
    dumpPkgs = parseLaTeXFile >=> \case
      Left err -> error $ show err
      Right doc -> mapM_ print . packageNames $ doc
    indexDb file = do
      entries <- parseTlpdbFile file
      print . length $ entries
      print . tlpdbHeaders . head $ entries


packageNames :: LaTeX -> Set Text
packageNames = Set.fromList . (Text.splitOn "," <=< (pkgName =<<) <=< lookForCommand "usepackage")
  where
    pkgName (FixArg arg) = pure $ render arg
    pkgName _            = []
