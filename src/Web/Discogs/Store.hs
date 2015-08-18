module Web.Discogs.Store where

import           Control.Monad
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Copy
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)


class Storable a where
    getQuery :: [a] -> Query
    toRow :: a -> ByteString
    avoid :: a -> Maybe String


store :: (Show a, Storable a) => String -> [a] -> IO ()
store cons ars = do
    conn <- connectPostgreSQL $ B.pack cons
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
