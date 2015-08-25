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
parseArtist (Element "artist" [] childs) = foldl' (flip parseArtist') (ArtistRelation "" "" "" "") childs
parseArtist _ = undefined

parseArtist' :: UNode ByteString -> ArtistRelation -> ArtistRelation
parseArtist' (Element "id" [] txt) = artistRelId .~ getTexts txt
parseArtist' (Element "anv" [] txt) = artistRelAnv .~ getTexts txt
parseArtist' (Element "join" [] txt) = artistRelJoin .~ getTexts txt
parseArtist' (Element "role" [] txt) = artistRelRole .~ getTexts txt
parseArtist' _ = id
