module Main where

import           System.Environment
import           Text.XML.Expat.Tree
import qualified Data.ByteString.Lazy as LB

import           Web.Discogs.Build
import           Web.Discogs.Store
import           Web.Discogs.Artist


main :: IO ()
main = do
    [f] <- getArgs
    contents <- LB.readFile f

    let artists :: [Artist]
        artists = build $ parse defaultParseOptions contents

    store "dbname='discogs2'" artists
