{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Web.Discogs.Artist
  ( Artist(..)
  ) where

import           Data.Monoid
import           Data.Maybe (mapMaybe)
import           Data.List (foldl')
import           Lens.Simple
import           Text.XML.Expat.Tree (NodeG(..), UNode)
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)

import           Web.Discogs.Build
import           Web.Discogs.Store


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

    toRow (Artist i n r d p us as gs ms ns) = (B.intercalate "\t"
        [quote i, quote n, quote r, quote d, quote p,
         quotes us, quotes as, quotes gs, quotes ms, quotes ns]) `B.snoc` '\n'
      where
        quote "" = "\\N"
        quote s = B.concatMap match s

        match '\\' = ""
        match '\n' = "\\n"
        match '\t' = "\\t"
        match x = B.singleton x

        quotes [] = "\\N"
        quotes l = "{" <> (B.intercalate "," $ map quoteA l) <> "}"

        quoteA s = "\"" <> B.concatMap matchA s <> "\""

        matchA '\"' = "\\\\\""
        matchA '\\' = ""
        matchA '\n' = "\\n"
        matchA '\t' = "\\t"
        matchA x = B.singleton x

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
getText = B.concat . mapMaybe pickText
  where
    pickText (Text s) = Just s
    pickText _ = error "Nested node in your text."
