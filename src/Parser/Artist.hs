{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Parser.Artist
  ( parse
  , Artist(..)
  ) where

import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Lens.Simple
import Text.XML.Expat.Tree (NodeG(..), XMLParseError, UNode)
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)


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


parse :: (UNode ByteString, Maybe XMLParseError) -> [Artist]
parse = parseArtists . fst

emptyArtist :: Artist
emptyArtist = Artist "" "" "" "" "" [] [] [] [] []

parseArtists :: UNode ByteString -> [Artist]
parseArtists (Element "artists" [] childs) = mapMaybe parseArtist childs
parseArtists _ = error "Couldn't find 'artists' tag."

parseArtist :: UNode ByteString -> Maybe Artist
parseArtist (Element "artist" [] childs) = Just $ foldl' parseArtist' emptyArtist childs
parseArtist (Text _) = Nothing
parseArtist _ = error "Couldn't find 'artist' tag."

parseArtist' :: Artist -> UNode ByteString -> Artist
parseArtist' a (Element "id" [] txt) = a & artistId .~ getText txt
parseArtist' a (Element "name" [] txt) = a & artistName .~ getText txt
parseArtist' a (Element "realname" [] txt) = a & artistRealName .~ getText txt
parseArtist' a (Element "profile" [] txt) = a & artistProfile .~ getText txt
parseArtist' a (Element "data_quality" [] txt) = a & artistQuality .~ getText txt
parseArtist' a (Element "namevariations" [] ns) = a & artistNameVars .~ getNodes "name" ns
parseArtist' a (Element "aliases" [] ns) = a & artistAliases .~ getNodes "name" ns
parseArtist' a (Element "members" [] ns) = a & artistMembers .~ getNodes "name" ns
parseArtist' a (Element "groups" [] ns) = a & artistGroups .~ getNodes "name" ns
parseArtist' a (Element "urls" [] ns) = a & artistUrls .~ getNodes "url" ns
parseArtist' a _ = a

getNodes :: ByteString -> [UNode ByteString] -> [ByteString]
getNodes tag = mapMaybe pickName
  where
      pickName (Element tag' [] txt) = if tag' == tag
                                           then Just $ getText txt
                                           else Nothing
      pickName _ = Nothing

getText :: [UNode ByteString] -> ByteString
getText = SB.concat . mapMaybe pickText
  where
    pickText (Text s) = Just s
    pickText _ = error "Nested node in your text."
