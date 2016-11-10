{-# LANGUAGE OverloadedStrings #-}
module Discogs.Store
 ( Escapable(..)
 , TableInfo(..)
 , Store(..)
 , Table(..)
 , store
 , escapeRow
 ) where

import Data.Monoid
import Data.List (intercalate, intersperse)
import Data.String
import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Copy
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toChunks)
import Data.ByteString.Internal (c2w)
import Data.ByteString.Builder as B (Builder, toLazyByteString, charUtf8)
import Data.ByteString.Builder.Prim as P


data TableInfo = TableInfo
  { tableName :: String
  , tableColumns :: [String] }

data Store a = Store
  { getStore :: [a]
  , getName :: String
  , getTables :: [TableInfo] }

class Table a where
    toRows :: a -> [Builder]
    avoid :: a -> Maybe String


store :: (Show a, Table a) => String -> Store a -> IO ()
store cns storable = do
    putStrLn $ "Parsing " ++ getName storable ++ "..."
    let tables = getTables storable

    conns <- forM tables $ \table -> do
        conn <- connectPostgreSQL $ pack cns
        begin conn
        _ <- execute_ conn $ fromString ("TRUNCATE " <> tableName table)
        copy_ conn $ toQuery table
        return conn

    forM_ (getStore storable) $ \val ->
        case avoid val of
            Just er -> do
                putStr "Avoiding entry for "
                putStr er
                putStr ": "
                print val
            Nothing -> do
                let rows = toRows val
                forM_ (zip conns rows) $ \(c, r) ->
                    mapM_ (putCopyData c) (toChunks $ toLazyByteString r)

    forM_ (zip tables conns) $ \(table, conn) -> do
        n <- putCopyEnd conn
        commit conn
        putStrLn $ tableName table <> " = " <> show n

toQuery :: TableInfo -> Query
toQuery (TableInfo n cs) = fromString $
    "COPY " ++ n ++ " (" ++
        intercalate "," cs ++
    ") FROM STDIN"


-- | Escape text data for use in a COPY FROM statement.
--   Basically a ByteString Builder that replaces some chars.
class Escapable b where
    escape :: b -> Builder
    escapeList :: [b] -> Builder


instance Escapable ByteString where
    escape "" = B.charUtf8 '\\' <> B.charUtf8 'N'
    escape s = primMapByteStringBounded replace s
      where
        replace =
            condB (> c2w '\\') (liftFixedToBounded word8) $
            condB (== c2w '\\') emptyB $
            condB (== c2w '\n') (fixed2 ('\\', 'n')) $
            condB (== c2w '\t') (fixed2 ('\\', 't')) $
            condB (== c2w '\r') (fixed2 ('\\', 'r')) $
            (liftFixedToBounded word8)

    escapeList [] = B.charUtf8 '\\' <> B.charUtf8 'N'
    escapeList l = B.charUtf8 '{' <>
                       (mconcat . intersperse (B.charUtf8 ',') . map wrap $ l) <>
                   B.charUtf8 '}'
      where
        wrap s = B.charUtf8 '\"' <>
                     primMapByteStringBounded replace s <>
                 B.charUtf8 '\"'
        replace =
            condB (> c2w '\\') (liftFixedToBounded word8) $
            condB (== c2w '\\') emptyB $
            condB (== c2w '\"') (fixed3 ('\\', ('\\', '\"'))) $
            condB (== c2w '\n') (fixed2 ('\\', 'n')) $
            condB (== c2w '\t') (fixed2 ('\\', 't')) $
            condB (== c2w '\r') (fixed2 ('\\', 'r')) $
            (liftFixedToBounded word8)


{-# INLINE fixed2 #-}
fixed2 :: (Char, Char) -> BoundedPrim a
fixed2 x = liftFixedToBounded $ const x >$< char7 >*< char7

{-# INLINE fixed3 #-}
fixed3 :: (Char, (Char, Char)) -> BoundedPrim a
fixed3 x = liftFixedToBounded $ const x >$< char7 >*< char7 >*< char7


escapeRow :: [Builder] -> Builder
escapeRow as = mconcat (intersperse (B.charUtf8 '\t') as) <> B.charUtf8 '\n'
