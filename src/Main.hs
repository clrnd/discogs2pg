module Main where

import           System.Environment
import           Text.XML.Expat.SAX
import qualified Data.ByteString.Lazy as LB

import qualified Parser.Artist as PA
import qualified Store.Artist as SA


main :: IO ()
main = do
    [f] <- getArgs
    contents <- LB.readFile f

    let artists = PA.parse $ parse opts contents

    SA.store "dbname='discogs2'" artists
  where
    opts = ParseOptions Nothing Nothing
