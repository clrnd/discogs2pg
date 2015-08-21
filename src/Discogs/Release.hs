{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Discogs.Release
  ( Release
  ) where

import Lens.Simple
import Data.Maybe
import Data.List (foldl')
import Data.Foldable (foldMap)
import Text.XML.Expat.Tree (NodeG(..), UNode, isElement)
import Data.ByteString (ByteString)

import Discogs.Build
import Discogs.Store


data Release = Release
  { _releaseId :: ByteString
  , _releaseMasterId :: ByteString
  , _releaseStatus :: ByteString
  , _releaseTitle :: ByteString
  , _releaseCountry :: ByteString
  , _releaseDate :: ByteString
  , _releaseQuality :: ByteString
  , _releaseNotes :: ByteString
  , _releaseArtists :: [ReleaseArtist]
  , _releaseExArtists :: [ReleaseArtist]
  , _releaseLabels :: [ReleaseLabel]
  , _releaseFormats :: [ReleaseFormat]
  , _releaseTracks :: [ReleaseTrack]
  , _releaseIdentifiers :: [ReleaseIdentifier]
  , _releaseVideos :: [ReleaseVideo]
  , _releaseCompanies :: [ReleaseCompany]
  , _releaseGenres :: [ByteString]
  , _releaseStyles :: [ByteString] }
  deriving Show

data ReleaseArtist = ReleaseArtist
  { _reArtId :: ByteString
  , _reArtAnv :: ByteString
  , _reArtJoin :: ByteString
  , _reArtRole :: ByteString }
  deriving Show

data ReleaseLabel = ReleaseLabel
  { _reLabLabel :: ByteString
  , _reLabCatno :: ByteString }
  deriving Show

data ReleaseFormat = ReleaseFormat
  { _reFmtName :: ByteString
  , _reFmtText :: ByteString
  , _reFmtQty :: ByteString
  , _reFmtDescriptions :: [ByteString] }
  deriving Show

data ReleaseTrack = ReleaseTrack
  { _reTrkTitle :: ByteString
  , _reTrkPosition :: ByteString
  , _reTrkDuration :: ByteString
  , _reTrkArtists :: [ReleaseArtist]
  , _reTrkExArtists :: [ReleaseArtist] }
  deriving Show

data ReleaseIdentifier = ReleaseIdentifier
  { _reIdtDescription :: ByteString
  , _reIdtType :: ByteString
  , _reIdtValue :: ByteString }
  deriving Show

data ReleaseVideo = ReleaseVideo
  { _reVidDuration :: ByteString
  , _reVidSrc :: ByteString
  , _reVidTitle :: ByteString }
  deriving Show

data ReleaseCompany = ReleaseCompany
  { _reComId :: ByteString
  , _reComCatno :: ByteString
  , _reComEntityType :: ByteString
  , _reComEntityName :: ByteString }
  deriving Show

$(makeLenses ''Release)
$(makeLenses ''ReleaseArtist)
$(makeLenses ''ReleaseLabel)
$(makeLenses ''ReleaseFormat)
$(makeLenses ''ReleaseTrack)
$(makeLenses ''ReleaseIdentifier)
$(makeLenses ''ReleaseVideo)
$(makeLenses ''ReleaseCompany)

instance Storable Release where
    getName _ = "releases"

    avoid = const Nothing

    toRows (Release i m s t c d q n as es ls fs ts is vs cs gs ss) = [
          escapeRow [escape i, escape m, escape s, escape t,
                     escape c, escape d, escape q, escape n,
                     escapeList gs, escapeList ss]
        , innerTable mkArtist as
        , innerTable mkArtist es
        , innerTable mkLabel ls
        , innerTable mkFormat fs
        , innerTable mkTracks ts
        , foldMap mksTrackArtists ts
        , foldMap mksTrackExArtists ts
        , innerTable mkIdentifier is
        , innerTable mkVideo vs
        , innerTable mkCompany cs
        ]
      where
        innerTable f = foldMap (escapeRow . f)

        mkArtist (ReleaseArtist i' a' j' r') =
            [escape i, escape i', escape a', escape j', escape r']
        mkLabel (ReleaseLabel l' c') =
            [escape i, escape l', escape c']
        mkFormat (ReleaseFormat n' t' q' ds') =
            [escape i, escape n', escape t', escape q', escapeList ds']
        mkTracks (ReleaseTrack t' p' d' _ _) =
            [escape i, escape t', escape p', escape d']
        mksTrackArtists (ReleaseTrack _ _ _ as' _) =
            foldMap (escapeRow . mkArtist) as'
        mksTrackExArtists (ReleaseTrack _ _ _ _ es') =
            foldMap (escapeRow . mkArtist) es'
        mkIdentifier (ReleaseIdentifier d' t' v') =
            [escape i, escape d', escape t', escape v']
        mkVideo (ReleaseVideo d' s' t') =
            [escape i, escape d', escape s', escape t']
        mkCompany (ReleaseCompany i' c' et' en') =
            [escape i, escape i', escape c', escape et', escape en']

    getTables _ = [
          TableInfo "release" ["id", "master_id", "status",
                               "title", "country", "released", "data_quality",
                               "notes", "genres", "styles"]
        , TableInfo "releases_artists" ["release_id", "artist_id",
                                        "anv", "join_relation", "role"]
        , TableInfo "releases_extraartists" ["release_id", "artist_id",
                                             "anv", "join_relation", "role"]
        , TableInfo "releases_labels" ["release_id", "label", "catno"]
        , TableInfo "releases_formats" ["release_id", "format_name",
                                        "format_text", "qty", "descriptions"]
        , TableInfo "track" ["release_id", "title",
                             "position", "duration"]
        , TableInfo "tracks_artists" ["release_id", "artist_id",
                                      "anv", "join_relation", "role"]
        , TableInfo "tracks_extraartists" ["release_id", "artist_id",
                                           "anv", "join_relation", "role"]
        , TableInfo "releases_identifiers" ["release_id", "description",
                                            "type", "value"]
        , TableInfo "releases_videos" ["release_id", "duration",
                                       "src", "title"]
        , TableInfo "releases_companies" ["release_id", "company_id",
                                          "catno", "entity_type",
                                          "entity_type_name"]
        ]

instance Buildable Release where
    build = parseReleases . fst

emptyRelease :: Release
emptyRelease = Release
  { _releaseId=""
  , _releaseMasterId=""
  , _releaseStatus=""
  , _releaseTitle=""
  , _releaseCountry=""
  , _releaseDate=""
  , _releaseQuality=""
  , _releaseNotes=""
  , _releaseArtists=[]
  , _releaseExArtists=[]
  , _releaseLabels=[]
  , _releaseFormats=[]
  , _releaseTracks=[]
  , _releaseIdentifiers=[]
  , _releaseVideos=[]
  , _releaseCompanies=[]
  , _releaseGenres=[]
  , _releaseStyles=[] }

parseReleases :: UNode ByteString -> [Release]
parseReleases (Element "releases" [] childs) = map parseRelease $ filter isElement childs
parseReleases _ = error "Couldn't find 'releases' tag."

parseRelease :: UNode ByteString -> Release
parseRelease (Element "release" attrs childs) = foldl' (flip parseRelease') newRelease childs
  where
    newRelease = foldl' (flip getAttrs) emptyRelease attrs
    getAttrs ("id", s) = releaseId .~ s
    getAttrs ("status", s) = releaseStatus .~ s
    getAttrs _ = id
parseRelease _ = error "Couldn't find 'release' tag."

parseRelease' :: UNode ByteString -> Release -> Release
parseRelease' (Element "title" [] txt) = releaseTitle .~ getTexts txt
parseRelease' (Element "master_id" [] txt) = releaseMasterId .~ getTexts txt
parseRelease' (Element "country" [] txt) = releaseCountry .~ getTexts txt
parseRelease' (Element "released" [] txt) = releaseDate .~ getTexts txt
parseRelease' (Element "notes" [] txt) = releaseNotes .~ getTexts txt
parseRelease' (Element "data_quality" [] txt) = releaseQuality .~ getTexts txt
parseRelease' (Element "genres" [] ns) = releaseGenres .~ getNodes "genre" ns
parseRelease' (Element "styles" [] ns) = releaseStyles .~ getNodes "style" ns
parseRelease' (Element "artists" [] ns) = releaseArtists .~ (map parseArtist $ filter isElement ns)
parseRelease' (Element "extraartists" [] ns) = releaseExArtists .~ (map parseArtist $ filter isElement ns)
parseRelease' (Element "labels" [] ns) = releaseLabels .~ (map parseLabel $ filter isElement ns)
  where
    parseLabel (Element "label" attrs _) = foldl' (flip getAttrs) (ReleaseLabel "" "") attrs
    parseLabel _ = undefined
    getAttrs ("catno", s) = reLabCatno .~ s
    getAttrs ("name", s) = reLabLabel .~ s
    getAttrs _ = id
parseRelease' (Element "formats" [] ns) = releaseFormats .~ (map parseFormat $ filter isElement ns)
  where
    parseFormat (Element "format" attrs ns') = foldl' (flip getAttrs) (ReleaseFormat "" "" "" []) attrs
                                             & reFmtDescriptions .~ getDescs ns'
    parseFormat _ = undefined
    getAttrs ("qty", s) = reFmtQty .~ s
    getAttrs ("name", s) = reFmtName .~ s
    getAttrs ("text", s) = reFmtText .~ s
    getAttrs _ = id
    getDescs = concatMap (\(Element _ _ ns'') -> getNodes "description" ns'') . filter isElement
parseRelease' (Element "identifiers" [] ns) = releaseIdentifiers .~ (map parseIdentifier $ filter isElement ns)
  where
    parseIdentifier (Element "identifier" attrs _) =
        foldl' (flip getAttrs) (ReleaseIdentifier "" "" "") attrs
    parseIdentifier _ = undefined
    getAttrs ("description", s) = reIdtDescription .~ s
    getAttrs ("type", s) = reIdtType .~ s
    getAttrs ("value", s) = reIdtValue .~ s
    getAttrs _ = id
parseRelease' (Element "videos" [] ns) = releaseVideos .~ (map parseVideo $ filter isElement ns)
  where
    parseVideo (Element "video" attrs ns') =
        foldl' (flip getAttrs) (ReleaseVideo "" "" "") attrs
      & reVidTitle .~ (getTexts . snd . fromJust $ lookUpNode "title" ns')
    parseVideo _ = undefined
    getAttrs ("duration", s) = reVidDuration .~ s
    getAttrs ("src", s) = reVidSrc .~ s
    getAttrs _ = id
parseRelease' (Element "companies" [] ns) = releaseCompanies .~ (map parseCompany $ filter isElement ns)
  where
    parseCompany (Element "company" [] ns') = foldl' (flip parseCompany') (ReleaseCompany "" "" "" "") ns'
    parseCompany _ = undefined
    parseCompany' (Element "id" [] txt) = reComId .~ getTexts txt
    parseCompany' (Element "catno" [] txt) = reComCatno .~ getTexts txt
    parseCompany' (Element "entity_type" [] txt) = reComEntityType .~ getTexts txt
    parseCompany' (Element "entity_type_name" [] txt) = reComEntityName .~ getTexts txt
    parseCompany' _ = id
parseRelease' (Element "tracklist" [] ns) = releaseTracks .~ (map parseTrack $ filter isElement ns)
  where
    parseTrack (Element "track" [] ns') = foldl' (flip parseTrack') (ReleaseTrack "" "" "" [] []) ns'
    parseTrack _ = undefined
    parseTrack' (Element "position" [] txt) = reTrkPosition .~ getTexts txt
    parseTrack' (Element "title" [] txt) = reTrkTitle .~ getTexts txt
    parseTrack' (Element "duration" [] txt) = reTrkDuration .~ getTexts txt
    parseTrack' (Element "artists" [] ns'') = reTrkArtists .~ (map parseArtist $ filter isElement ns'')
    parseTrack' (Element "extraartists" [] ns'') = reTrkExArtists .~ (map parseArtist $ filter isElement ns'')
    parseTrack' _ = id
parseRelease' _ = id

parseArtist :: UNode ByteString -> ReleaseArtist
parseArtist (Element "artist" [] childs) = foldl' (flip parseArtist') (ReleaseArtist "" "" "" "") childs
parseArtist _ = undefined

parseArtist' :: UNode ByteString -> ReleaseArtist -> ReleaseArtist
parseArtist' (Element "id" [] txt) = reArtId .~ getTexts txt
parseArtist' (Element "anv" [] txt) = reArtAnv .~ getTexts txt
parseArtist' (Element "join" [] txt) = reArtJoin .~ getTexts txt
parseArtist' (Element "role" [] txt) = reArtRole .~ getTexts txt
parseArtist' _ = id
