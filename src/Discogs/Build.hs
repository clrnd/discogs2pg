module Discogs.Build
  ( Buildable(..)
  , getNodes
  , getTexts
  , lookUpNode
  ) where

import Data.Monoid
import Data.List (find)
import Data.Maybe
import Text.XML.Expat.Tree
import Data.ByteString (ByteString)


class Buildable a where
    build :: (UNode ByteString, Maybe XMLParseError) -> [a]

getNodes :: GenericXMLString a => a -> [UNode a] -> [a]
getNodes tag = mapMaybe pickName
  where
      pickName (Element tag' [] txt) = if tag' == tag
                                           then Just $ getTexts txt
                                           else Nothing
      pickName _ = Nothing

getTexts :: GenericXMLString a => [UNode a] -> a
getTexts = mconcat . mapMaybe pickText
  where
    pickText (Text s) = Just s
    pickText _ = error "Nested node in your text."

lookUpNode :: GenericXMLString a => a -> [UNode a]
              -> Maybe ([(a, a)], [UNode a])
lookUpNode name nodes = fmap get' $ find (isNamed name) nodes
  where
      get' (Element _ attrs children) = (attrs, children)
      get' _ = undefined
