{-# LANGUAGE PackageImports #-}
module Tex.Tlpdb where

import           "base" Data.Char                              (ord)
import           "base" Data.Either                            (lefts, rights)
import           "base" Data.Word                              (Word8)

import           "containers" Data.Map                         (Map)
import qualified "containers" Data.Map                         as Map

import           "attoparsec" Data.Attoparsec.ByteString
import qualified "attoparsec" Data.Attoparsec.ByteString       as AP
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
parseTlpdbEntries = many1 parseTlpdbEntry

parseTlpdbEntry :: Parser TlpdbEntry
parseTlpdbEntry = do
    lns <- (many1 parseLine <* char '\n') <?> "entry"
    pure $ TlpdbEntry (Map.fromListWith cat . lefts $ lns) (rights lns)
  where
    cat a b = a ++ '\n' : b

parseLine :: Parser (Either (String, String) FilePath)
parseLine = prsr <?> "parseLine"
  where
    prsr = do
        key <- BS.unpack <$> AP.takeWhile (not . isWhitespace) <* char ' '
        value <- restOfLine
        pure $ if key == ""
                then Right value
                else Left (key, value)

isWhitespace :: Word8 -> Bool
isWhitespace w = w == toEnum (ord ' ') || endOfLine w

restOfLine :: Parser String
restOfLine = fmap BS.unpack (takeWhile1 (not . endOfLine)) <* char '\n'


endOfLine :: Word8 -> Bool
endOfLine w = w == 13 || w == 10

