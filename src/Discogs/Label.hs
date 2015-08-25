{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Discogs.Label
  ( Label
  , labelStore
  ) where

import Lens.Simple
import Data.List (foldl')
import Text.XML.Expat.Tree (NodeG(..), UNode, isElement)
import Data.ByteString (ByteString)

import Discogs.Build
import Discogs.Store


data Label = Label
  { _labelId :: ByteString
  , _labelName :: ByteString
  , _labelContactInfo :: ByteString
  , _labelParentLabel :: ByteString
  , _labelProfile :: ByteString
  , _labelQuality :: ByteString
  , _labelUrls :: [ByteString]
  , _labelSublabels :: [ByteString] }
  deriving Show

$(makeLenses ''Label)


labelStore :: [Label] -> Store Label
labelStore x = Store
  { getStore=x
  , getName="labels"
  , getTables=[
      TableInfo "label" ["id", "name", "contactinfo",
                         "parent_label", "profile", "data_quality",
                         "urls", "sublabels"]
    ]
  }

instance Table Label where
    avoid = const Nothing

    toRows (Label i n c p f q us ss) = [
          escapeRow [escape i, escape n, escape c,
                     escape p, escape f, escape q,
                     escapeList us, escapeList ss]
        ]

instance Buildable Label where
    build  = parseLabels

emptyLabel :: Label
emptyLabel = Label "" "" "" "" "" "" [] []

parseLabels :: UNode ByteString -> [Label]
parseLabels (Element "labels" [] childs) = map parseLabel $ filter isElement childs
parseLabels _ = error "Couldn't find 'labels' tag."

parseLabel :: UNode ByteString -> Label
parseLabel (Element "label" [] childs) = foldl' (flip parseLabel') emptyLabel childs
parseLabel _ = error "Couldn't find 'label' tag."

parseLabel' :: UNode ByteString -> Label -> Label
parseLabel' (Element "id" [] txt) = labelId .~ getTexts txt
parseLabel' (Element "name" [] txt) = labelName .~ getTexts txt
parseLabel' (Element "contactinfo" [] txt) = labelContactInfo .~ getTexts txt
parseLabel' (Element "profile" [] txt) = labelProfile .~ getTexts txt
parseLabel' (Element "parentLabel" [] txt) = labelParentLabel .~ getTexts txt
parseLabel' (Element "data_quality" [] txt) = labelQuality .~ getTexts txt
parseLabel' (Element "urls" [] ns) = labelUrls .~ getNodes "url" ns
parseLabel' (Element "sublabels" [] ns) = labelSublabels .~ getNodes "label" ns
parseLabel' _ = id
