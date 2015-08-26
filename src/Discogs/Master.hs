{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Discogs.Master
  ( Master
  , masterStore
  ) where

import Lens.Simple
import Data.List (foldl')
import Data.Foldable (foldMap)
import Text.XML.Expat.Tree (NodeG(..), UNode, isElement)
import Data.ByteString (ByteString)

import Discogs.ArtistRelation
import Discogs.Build
import Discogs.Store


data Master = Master
  { _masterId :: ByteString
  , _masterTitle :: ByteString
  , _masterMain :: ByteString
  , _masterYear :: ByteString
  , _masterNotes :: ByteString
  , _masterQuality :: ByteString
  , _masterGenres :: [ByteString]
  , _masterStyles :: [ByteString]
  , _masterArtists :: [ArtistRelation] }
  deriving Show

$(makeLenses ''Master)


masterStore :: [Master] -> Store Master
masterStore x = Store
  { getStore=x
  , getName="masters"
  , getTables=[
      TableInfo "master" ["id", "title", "main_release",
                         "year", "notes", "data_quality",
                         "genres", "styles"]
    , TableInfo "master_artist" ["master_id", "artist_id",
                                 "anv", "join_relation", "role"]
    ]
  }

instance Table Master where
    avoid = const Nothing

    toRows (Master i t m y n q gs ss as) = [
          escapeRow [escape i, escape t, escape m,
                   escape y, escape n, escape q,
                   escapeList gs, escapeList ss]
        , innerTable mkArtist as
        ]
      where
        innerTable f = foldMap (escapeRow . f)
        mkArtist (ArtistRelation i' a' j' r') =
            [escape i, escape i', escape a', escape j', escape r']

instance Buildable Master where
    build  = parseMasters

emptyMaster :: Master
emptyMaster = Master "" "" "" "" "" "" [] [] []

parseMasters :: UNode ByteString -> [Master]
parseMasters (Element "masters" [] childs) = map parseMaster $ filter isElement childs
parseMasters _ = error "Couldn't find 'masters' tag."

parseMaster :: UNode ByteString -> Master
parseMaster (Element "master" attrs childs) = foldl' (flip parseMaster') newMaster childs
  where
    newMaster = foldl' (flip getAttrs) emptyMaster attrs
    getAttrs ("id", s) = masterId .~ s
    getAttrs _ = id
parseMaster _ = error "Couldn't find 'master' tag."

parseMaster' :: UNode ByteString -> Master -> Master
parseMaster' (Element "title" [] txt) = masterTitle .~ getTexts txt
parseMaster' (Element "main_release" [] txt) = masterMain .~ getTexts txt
parseMaster' (Element "year" [] txt) = masterYear .~ getTexts txt
parseMaster' (Element "notes" [] txt) = masterNotes .~ getTexts txt
parseMaster' (Element "data_quality" [] txt) = masterQuality .~ getTexts txt
parseMaster' (Element "genres" [] ns) = masterGenres .~ getNodes "genre" ns
parseMaster' (Element "styles" [] ns) = masterStyles .~ getNodes "style" ns
parseMaster' (Element "artists" [] ns) = masterArtists .~ (map parseArtist $ filter isElement ns)
parseMaster' _ = id
