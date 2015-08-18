{-# LANGUAGE OverloadedStrings #-}
module Discogs.Store where

import Data.Monoid
import Data.List (intersperse)
import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Copy
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Internal (w2c)
import Data.ByteString.Builder as B (Builder, toLazyByteString, charUtf8)
import Data.ByteString.Builder.Prim as P


class Storable a where
    getQuery :: [a] -> Query
    toRow :: a -> ByteString
    avoid :: a -> Maybe String


store :: (Show a, Storable a) => String -> [a] -> IO ()
store cons ars = do
    conn <- connectPostgreSQL $ pack cons
    copy_ conn $ getQuery ars

    forM_ ars $ \a ->
        case avoid a of
            Just er -> do
                putStr "Avoiding artist for "
                putStr er
                putStr ": "
                print a
            Nothing -> do
                putCopyData conn $ toRow a

    i <- putCopyEnd conn
    putStr "Parsed: " >> print i


class Escapable b where
    escape :: b -> Builder
    escapeList :: [b] -> Builder


instance Escapable ByteString where
    escape "" = B.charUtf8 '\\' <> B.charUtf8 'N'
    escape s = primMapByteStringBounded (w2c >$< replace) s
      where
        replace =
            condB (== '\\') emptyB $
            condB (== '\n') (fixed2 ('\\', 'n')) $
            condB (== '\t') (fixed2 ('\\', 't')) $
            P.charUtf8

    escapeList [] = B.charUtf8 '\\' <> B.charUtf8 'N'
    escapeList l = B.charUtf8 '{' <>
                       (mconcat . intersperse (B.charUtf8 ',') . map wrap $ l) <>
                   B.charUtf8 '}'
      where
        wrap s = B.charUtf8 '\"' <>
                     primMapByteStringBounded (w2c >$< replace) s <>
                 B.charUtf8 '\"'
        replace =
            condB (== '\\') emptyB $
            condB (== '\"') (fixed3 ('\\', ('\\', '\"'))) $
            condB (== '\n') (fixed2 ('\\', 'n')) $
            condB (== '\t') (fixed2 ('\\', 't')) $
            P.charUtf8


{-# INLINE fixed2 #-}
fixed2 :: (Char, Char) -> BoundedPrim Char
fixed2 x = liftFixedToBounded $ const x >$< char7 >*< char7

{-# INLINE fixed3 #-}
fixed3 :: (Char, (Char, Char)) -> BoundedPrim Char
fixed3 x = liftFixedToBounded $ const x >$< char7 >*< char7 >*< char7


escapeRow :: [Builder] -> ByteString
escapeRow as = toStrict . toLazyByteString $
    mconcat (intersperse (B.charUtf8 '\t') as) <> B.charUtf8 '\n'
