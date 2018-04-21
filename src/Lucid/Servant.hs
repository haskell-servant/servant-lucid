{-# LANGUAGE OverloadedStrings #-}

-- | Some helper functions for creating values in
-- [lucid](https://hackage.haskell.org/package/lucid) DSLs that work
-- with [servant](https://hackage.haskell.org/package/servant).
module Lucid.Servant
  ( absHref_
  , relHref_
  ) where

import Data.Monoid ((<>))
import Lucid (Attribute)
import Lucid.Html5 (href_)
import Servant.API (toUrlPiece)
import Servant.Utils.Links (Link)

-- | Create an `href` attribute from a 'Link', with leading '/'.
--
-- "servant" ensures that any 'Link' is valid within an API.
-- This function ensures it is possible to navigate to that endpoint from
-- a page which shares a root with that API.
absHref_ :: Link -> Attribute
absHref_ = href_ . ("/" <>) . toUrlPiece

-- | Create an `href` attribute from a 'Link', as a relative link.
--
-- "servant" ensures that any 'Link' is valid within an API.
-- Use this function if a relative link (no leading '/') is required.
relHref_ :: Link -> Attribute
relHref_ = href_ . toUrlPiece
