module Main where

import           System.Environment
import           Text.XML.Expat.Tree
import qualified Data.ByteString.Lazy as LB

import qualified Parser.Artist as PA
import qualified Store.Artist as SA


main :: IO ()
main = do
    [f] <- getArgs
    contents <- LB.readFile f

    let artists = PA.parse $ parse defaultParseOptions contents

    SA.store "dbname='discogs2'" artists
