{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Store.Artist
  ( store
  ) where

import           Data.Monoid
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Copy
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Char8 (ByteString)

import           Parser.Artist


store :: String -> [Artist] -> IO ()
store cons ars = do
    conn <- connectPostgreSQL $ BC.pack cons
    copy_ conn [sql|COPY artist (id, name, realname,
                                 data_quality, profile,
                                 urls, aliases, groups,
                                 members, namevariations)
                    FROM STDIN |]
    mapM_ (process conn) ars
    i <- putCopyEnd conn
    putStr "Artists parsed: " >> print i

process :: Connection -> Artist -> IO ()
process conn a = do
    case avoid a of
        Just er -> do
            putStr "Avoiding artist for "
            putStr er
            putStr ": "
            print a
        Nothing -> do
            putCopyData conn $ serialize a

avoid :: Artist -> Maybe String
avoid (Artist _ "" _ _ _ _ _ _ _ _) = Just "empty 'name'"
avoid _ = Nothing

serialize :: Artist -> ByteString
serialize (Artist i n r d p us as gs ms ns) = (BC.intercalate "\t"
    [quote i, quote n, quote r, quote d, quote p,
     quotes us, quotes as, quotes gs, quotes ms, quotes ns]) `BC.snoc` '\n'
  where
    quote "" = "\\N"
    quote s = BC.concatMap match s

    match '\\' = ""
    match '\n' = "\\n"
    match '\t' = "\\t"
    match x = BC.singleton x

    quotes [] = "\\N"
    quotes l = "{" <> (BC.intercalate "," $ map quoteA l) <> "}"

    quoteA s = "\"" <> BC.concatMap matchA s <> "\""

    matchA '\"' = "\\\\\""
    matchA '\\' = ""
    matchA '\n' = "\\n"
    matchA '\t' = "\\t"
    matchA x = BC.singleton x
