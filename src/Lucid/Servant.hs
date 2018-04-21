module Lucid.Servant
  ( safeHref_
  ) where

import Servant.API (toUrlPiece)
import Servant.Utils.Links (Link)
import Lucid (Attribute)
import Lucid.Html5 (href_)

-- | Create an `href` attribute from a 'Link'.
--
-- "servant" ensures that any 'Link' is valid.
safeHref_ :: Link -> Attribute
safeHref_ = href_ . toUrlPiece
