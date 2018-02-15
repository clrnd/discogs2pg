{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Codec.Compression.GZip (decompress)
import           Control.Concurrent
import           Text.XML.Expat.Tree
import qualified Data.ByteString.Lazy as LB

import           Discogs.Build
import           Discogs.Store
import           Discogs.Types
import           Options


main :: IO ()
main = do
    opts <- getOptions

    case fileOptions opts of
        Date d True -> importMany opts d True
        Date d False -> importMany opts d False
        Filename f -> importOne opts f

importMany :: Options -> Int -> Bool -> IO ()
importMany opts d agr = do
    let read_f = case (isGzip opts) of
                     True -> fmap decompress . LB.readFile
                     False -> LB.readFile

    let actions = map (process read_f) [ ("labels", run labelStore)
                                       , ("masters", run masterStore)
                                       , ("artists", run artistStore)
                                       , ("releases", run releaseStore) ]
    case agr of
        True -> mapM forkChildren actions >>= mapM_ takeMVar
        False -> sequence_ actions
  where
    process read_f (n, (MkRunnable f)) =
            read_f (format n) >>=
            store (connString opts) . f . build .  parseThrowing defaultParseOptions
    format s = "discogs_" ++ show d ++ "_" ++ s ++ ".xml" ++
               if isGzip opts then ".gz" else ""

    forkChildren act = do
        mvar <- newEmptyMVar
        _ <- forkFinally act (\e -> handleExc e >> putMVar mvar ())
        return mvar

    handleExc (Left err) = print err
    handleExc _ = return ()

importOne :: Options -> String -> IO ()
importOne opts filename = do
    contents <- case (isGzip opts) of
        True -> fmap decompress (LB.readFile filename)
        False -> LB.readFile filename

    let xml = parseThrowing defaultParseOptions contents

    case xml of
        Element "masters" _ _ -> storeF masterStore xml
        Element "artists" _ _ -> storeF artistStore xml
        Element "releases" _ _ -> storeF releaseStore xml
        Element "labels" _ _ -> storeF labelStore xml
        _ -> error "Couldn't figure out contents of file."

  where
    storeF f = store (connString opts) . f . build
