{-# LANGUAGE OverloadedStrings #-}
module Main where

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

    let cns = connString opts

    case fileOptions opts of
        Date d True -> importMany cns d True
        Date d False -> importMany cns d False
        Filename f -> importOne cns f

importMany :: String -> Int -> Bool -> IO ()
importMany cns d agr = do
    let actions = map process [ ("labels", run labelStore)
                              , ("masters", run masterStore)
                              , ("artists", run artistStore)
                              , ("releases", run releaseStore) ]
    case agr of
        True -> mapM forkChildren actions >>= mapM_ takeMVar
        False -> sequence_ actions
  where
    process (n, (MkRunnable f)) = LB.readFile (format n) >>=
                        store cns . f . build .
                        parseThrowing defaultParseOptions
    format s = "discogs_" ++ show d ++ "_" ++ s ++ ".xml"

    forkChildren act = do
        mvar <- newEmptyMVar
        _ <- forkFinally act (\e -> handleExc e >> putMVar mvar ())
        return mvar

    handleExc (Left err) = print err
    handleExc _ = return ()

importOne :: String -> String -> IO ()
importOne cns file = do
    contents <- LB.readFile file

    let xml = parseThrowing defaultParseOptions contents

    case xml of
        Element "masters" _ _ -> storeF masterStore xml
        Element "artists" _ _ -> storeF artistStore xml
        Element "releases" _ _ -> storeF releaseStore xml
        Element "labels" _ _ -> storeF labelStore xml
        _ -> error "Couldn't figure out contents of file."

  where
    storeF f = store cns . f . build
