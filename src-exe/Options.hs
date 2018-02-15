module Options
  ( getOptions
  , Options(..)
  , FileOptions(..)
  ) where

import Data.Monoid
import Options.Applicative
import Options.Applicative.Help hiding (fullDesc)


data Options = Options
  { connString :: String
  , fileOptions :: FileOptions
  , isGzip :: Bool }
  deriving Show

data FileOptions =
    Date Int Bool
  | Filename String
  deriving Show


getOptions :: IO Options
getOptions = customExecParser (prefs showHelpOnError) optsParserInfo

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
    (fullDesc
  <> progDesc "Discogs XML data dumps importer for PostgreSQL."
  <> (footerDoc . unChunk . vsepChunks $
      [ paragraph "Examples:"
      , paragraph "This will import the file \"artist.xml\" into the database named \"discogs\" in localhost."
      , paragraph "discogs2pg -c \"host=localhost dbname=discogs\" artists.xml"
      , paragraph "This one will do the same for all files named discogs_20150708_(artists|labels|masters|releases).xml"
      , paragraph "discogs2pg -c \"host=localhost dbname=discogs\" -d 20150708"]))

optsParser :: Parser Options
optsParser = Options <$> strOption (long "conn"
                                 <> short 'c'
                                 <> metavar "CONN"
                                 <> help "PostgreSQL libpq connection string")
                     <*> fileOptsParser
                     <*> switch (long "gzip"
                              <> short 'g'
                              <> help "Read compressed files.")

fileOptsParser :: Parser FileOptions
fileOptsParser =
        Date <$> option auto (long "date"
                           <> short 'd'
                           <> metavar "DATE"
                           <> help "Process all xml files dated DATE")
             <*> switch (long "aggressive"
                      <> help "Do it in parallel")
    <|> Filename <$> argument str (metavar "FILE"
                                <> help "Process a single file")
