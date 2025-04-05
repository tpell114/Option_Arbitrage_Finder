{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_options_arbitrage_finder (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "options_arbitrage_finder"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Options arbitrage finder"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
