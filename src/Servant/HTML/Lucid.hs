{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | An @HTML@ empty data type with `MimeRender` instances for @lucid@'s
-- `ToHtml` class and `Html` datatype.
-- You should only need to import this module for it's instances and the
-- `HTML` datatype.:
--
-- >>> type Eg = Get '[HTML] a
--
-- Will then check that @a@ has a `ToHtml` instance, or is `Html`.
module Servant.HTML.Lucid where

import           Data.Typeable      (Typeable)
import           Lucid              (ToHtml (..), renderBS)
import qualified Network.HTTP.Media as M
import           Servant.API        (Accept (..), MimeRender (..))

data HTML deriving Typeable

-- | @text/html;charset=utf-8@
instance Accept HTML where
    contentType _ = "text" M.// "html" M./: ("charset", "utf-8")

instance ToHtml a => MimeRender HTML a where
    mimeRender _ = renderBS . toHtml
