{-# LANGUAGE NamedFieldPuns #-}
module Main where

import "base" Control.Monad ((<=<))
import "base" Data.Maybe (listToMaybe)
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as Set

import "optparse-applicative" Options.Applicative (Parser, (<**>), info, helper, fullDesc, argument, str, metavar, execParser) 

import "HaTeX" Text.LaTeX.Base.Parser (parseLaTeXFile)
import "HaTeX" Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), lookForCommand)
import "HaTeX" Text.LaTeX.Base.Render (render)

import "text" Data.Text (Text)
import qualified "text" Data.Text as Text

data Args = Args
    { file :: FilePath
    }
  deriving (Eq, Ord, Show)

argsParser :: Parser Args
argsParser = Args
    <$> (argument str (metavar "FILE"))

opts = info (argsParser <**> helper) fullDesc

main :: IO ()
main = do
    Args{ file } <- execParser opts
    res <- parseLaTeXFile file
    case res of
      Left err -> error $ show err
      Right doc -> mapM_ print . packageNames $ doc

packageNames :: LaTeX -> Set Text
packageNames = Set.fromList . (Text.splitOn "," <=< (pkgName =<<) <=< lookForCommand "usepackage")
  where
    pkgName (FixArg arg) = pure $ render arg 
    pkgName _            = []
