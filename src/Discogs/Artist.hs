{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Discogs.Artist
  ( Artist(..)
  ) where

import Lens.Simple
import Text.XML.Expat.Tree (NodeG(..), UNode, isElement)
import Database.PostgreSQL.Simple.SqlQQ
import Data.ByteString (ByteString)

import Discogs.Build
import Discogs.Store


data Artist = Artist
  { _artistId :: ByteString
  , _artistName :: ByteString
  , _artistRealName :: ByteString
  , _artistQuality :: ByteString
  , _artistProfile :: ByteString
  , _artistUrls :: [ByteString]
  , _artistAliases :: [ByteString]
  , _artistGroups :: [ByteString]
  , _artistMembers :: [ByteString]
  , _artistNameVars :: [ByteString] }
  deriving Show

$(makeLenses ''Artist)

instance Storable Artist where
    avoid (Artist _ "" _ _ _ _ _ _ _ _) = Just "empty 'name'"
    avoid _ = Nothing

    toRow (Artist i n r d p us as gs ms ns) = escapeRow $
        [escape i, escape n, escape r, escape d, escape p,
         escapeList us, escapeList as, escapeList gs, escapeList ms, escapeList ns]

    getQuery _ = [sql|COPY artist (id, name, realname,
                                   data_quality, profile,
                                   urls, aliases, groups,
                                   members, namevariations)
                      FROM STDIN |]

instance Buildable Artist where
    build = parseArtists . fst

emptyArtist :: Artist
emptyArtist = Artist "" "" "" "" "" [] [] [] [] []

parseArtists :: UNode ByteString -> [Artist]
parseArtists (Element "artists" [] childs) = map parseArtist $ filter isElement childs
parseArtists _ = error "Couldn't find 'artists' tag."

parseArtist :: UNode ByteString -> Artist
parseArtist (Element "artist" [] childs) = foldr parseArtist' emptyArtist childs
parseArtist _ = error "Couldn't find 'artist' tag."

parseArtist' :: UNode ByteString -> Artist -> Artist
parseArtist' (Element "id" [] txt) = artistId .~ getTexts txt
parseArtist' (Element "name" [] txt) = artistName .~ getTexts txt
parseArtist' (Element "realname" [] txt) = artistRealName .~ getTexts txt
parseArtist' (Element "profile" [] txt) = artistProfile .~ getTexts txt
parseArtist' (Element "data_quality" [] txt) = artistQuality .~ getTexts txt
parseArtist' (Element "namevariations" [] ns) = artistNameVars .~ getNodes "name" ns
parseArtist' (Element "aliases" [] ns) = artistAliases .~ getNodes "name" ns
parseArtist' (Element "members" [] ns) = artistMembers .~ getNodes "name" ns
parseArtist' (Element "groups" [] ns) = artistGroups .~ getNodes "name" ns
parseArtist' (Element "urls" [] ns) = artistUrls .~ getNodes "url" ns
parseArtist' _ = id
