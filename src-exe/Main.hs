module Main where

import           System.Environment
import           Text.XML.Expat.Tree
import qualified Data.ByteString.Lazy as LB
import           Text.Show.Pretty

import           Discogs.Build
import           Discogs.Store
import           Discogs.Types


main :: IO ()
main = do
    [f] <- getArgs
    contents <- LB.readFile f

    let artists :: [Artist]
        artists = build $ parse defaultParseOptions contents

    --store "dbname='discogs2'" artists
    --mapM_ (putStrLn . ppShow) releases

    let releases :: [Release]
        releases = build $ parse defaultParseOptions contents

    store "dbname='discogs2'" releases
    --mapM_ (putStrLn . ppShow) releases
