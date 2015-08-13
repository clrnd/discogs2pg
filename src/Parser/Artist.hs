{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Parser.Artist
  ( parse
  , Artist(..)
  ) where

import Lens.Simple
import Control.Monad.State
import Text.XML.Expat.SAX (SAXEvent(..))
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

data ArtistState = ArtistState
  { _nested :: ArtistNested
  , _artist :: Artist
  , _buffer :: ByteString
  , _events :: [SAXEvent ByteString ByteString] }

data ArtistNested = None | Members | NameVars | Aliases | Groups

$(makeLenses ''Artist)
$(makeLenses ''ArtistState)


parse :: [SAXEvent ByteString ByteString] -> [Artist]
parse evs = evalState parseArtist initS
  where
    initS = ArtistState None emptyArtist SB.empty evs

emptyArtist :: Artist
emptyArtist = Artist SB.empty SB.empty SB.empty SB.empty SB.empty [] [] [] [] []

parseArtist :: State ArtistState [Artist]
parseArtist = do
    (ev:evs) <- use events
    events .= evs

    buffer' <- use buffer

    case ev of
        CharacterData txt -> do
            buffer <>= txt
            parseArtist

        StartElement el _ -> do
            case el of
                "members" -> nested .= Members
                "aliases" -> nested .= Aliases
                "groups" -> nested .= Groups
                "namevariations" -> nested .= NameVars
                "artist" -> artist .= emptyArtist
                _ -> return ()
            parseArtist

        EndElement "artists" -> return []

        EndElement "artist" -> do
            a <- use artist
            etc <- parseArtist
            return $ a : etc

        EndElement el -> do
            case el of
                "id" -> use nested >>= \case
                    Members -> return ()
                    _ -> artist . artistId .= buffer'

                "name" -> use nested >>= \case
                    Members -> do
                        m <- use $ artist . artistMembers
                        artist . artistMembers .= buffer' : m
                    Aliases -> do
                        m <- use $ artist . artistAliases
                        artist . artistAliases .= buffer' : m
                    Groups -> do
                        m <- use $ artist . artistGroups
                        artist . artistGroups .= buffer' : m
                    NameVars -> do
                        m <- use $ artist . artistNameVars
                        artist . artistNameVars .= buffer' : m
                    None -> artist . artistName .= buffer'

                "url" -> do
                        m <- use $ artist . artistUrls
                        artist . artistUrls .= buffer' : m
                "realname" -> artist . artistRealName .= buffer'
                "profile" -> artist . artistProfile .= buffer'
                "data_quality" -> artist . artistQuality .= buffer'

                "members" -> nested .= None
                "aliases" -> nested .= None
                "groups" -> nested .= None
                "namevariations" -> nested .= None
                _ -> return ()

            buffer .= SB.empty
            parseArtist

        _ -> parseArtist
