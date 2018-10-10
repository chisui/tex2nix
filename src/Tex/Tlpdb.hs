{-# LANGUAGE PackageImports #-}
module Tex.Tlpdb where

import           "base" Control.Applicative                    (liftA2)
import           "base" Data.Char                              (ord)
import           "base" Data.Maybe                             (fromMaybe)

import           "containers" Data.Map                         (Map)
import qualified "containers" Data.Map                         as Map

import           "attoparsec" Data.Attoparsec.ByteString
import           "attoparsec" Data.Attoparsec.ByteString.Char8 (char)

import qualified "bytestring" Data.ByteString.Char8            as BS


data TlpdbEntry = TlpdbEntry
    { tlpdbHeaders :: Map String String
    , tlpdbFiles   :: [FilePath]
    }
  deriving (Eq, Show)

parseTlpdbFile :: FilePath -> IO [TlpdbEntry]
parseTlpdbFile file = do
    res <- parseOnly parseTlpdbEntries <$> BS.readFile file
    case res of
        Left err -> fail err
        Right ex -> pure ex


parseTlpdbEntries :: Parser [TlpdbEntry]
parseTlpdbEntries = parseTlpdbEntry `sepBy` many1 (char '\n')

parseTlpdbEntry :: Parser TlpdbEntry
parseTlpdbEntry = do
    allLines <- Map.fromListWith (++) . (fmap . fmap) pure <$> parseLines
    let files = fromMaybe [] . Map.lookup "" $ allLines
    let headers = fmap unlines . Map.delete "" $ allLines
    pure $ TlpdbEntry headers files

parseLines :: Parser [(String, String)]
parseLines = parseLine `sepBy` char '\n'

parseLine :: Parser (String, String)
parseLine = liftA2 (,) (strTill ' ') (strTill '\n')
  where
    strTill = fmap BS.unpack . takeTill . (==) . toEnum . ord

