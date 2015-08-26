{-# LANGUAGE ExistentialQuantification #-}
module Discogs.Types
  ( module H
  , run
  , Runnable(..)
  ) where

import Discogs.Artist as H
import Discogs.Release as H
import Discogs.Label as H
import Discogs.Master as H

import Discogs.Build
import Discogs.Store


data Runnable = forall a.
    (Buildable a, Show a, Table a) =>
    MkRunnable ([a] -> Store a)

run :: (Buildable a, Show a, Table a) =>
        ([a] -> Store a)
        -> Runnable
run = MkRunnable
