module Web.Discogs.Build where

import Text.XML.Expat.Tree
import Data.ByteString (ByteString)


class Buildable a where
    build :: (UNode ByteString, Maybe XMLParseError) -> [a]
