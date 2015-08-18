module Discogs.Build where

import           Data.Maybe
import           Text.XML.Expat.Tree
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)


class Buildable a where
    build :: (UNode ByteString, Maybe XMLParseError) -> [a]

getNodes :: ByteString -> [UNode ByteString] -> [ByteString]
getNodes tag = mapMaybe pickName
  where
      pickName (Element tag' [] txt) = if tag' == tag
                                           then Just $ getTexts txt
                                           else Nothing
      pickName _ = Nothing

getTexts :: [UNode ByteString] -> ByteString
getTexts = B.concat . mapMaybe pickText
  where
    pickText (Text s) = Just s
    pickText _ = error "Nested node in your text."
