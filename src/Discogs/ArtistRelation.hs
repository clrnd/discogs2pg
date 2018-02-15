{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Discogs.ArtistRelation
  ( ArtistRelation(ArtistRelation)
  , parseArtist
  ) where

import Lens.Simple
import Data.List (foldl')
import Text.XML.Expat.Tree (NodeG(..), UNode)
import Data.ByteString (ByteString)

import Discogs.Build


data ArtistRelation = ArtistRelation
  { _artistRelId :: ByteString
  , _artistRelAnv :: ByteString
  , _artistRelJoin :: ByteString
  , _artistRelRole :: ByteString }
  deriving Show

$(makeLenses ''ArtistRelation)


parseArtist :: UNode ByteString -> ArtistRelation
parseArtist (Element "artist" _ childs) = foldl' (flip parseArtist') (ArtistRelation "" "" "" "") childs
parseArtist _ = undefined

parseArtist' :: UNode ByteString -> ArtistRelation -> ArtistRelation
parseArtist' (Element "id" _ txt) = artistRelId .~ getTexts txt
parseArtist' (Element "anv" _ txt) = artistRelAnv .~ getTexts txt
parseArtist' (Element "join" _ txt) = artistRelJoin .~ getTexts txt
parseArtist' (Element "role" _ txt) = artistRelRole .~ getTexts txt
parseArtist' _ = id
