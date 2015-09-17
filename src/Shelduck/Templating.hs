module Shelduck.Templating where

import           Data.Text
import           Data.Text.Lazy        (toStrict)
import qualified Data.Text.Lazy.IO     as TL
import           Data.UUID
import           Data.UUID.V4
import           Text.Hastache
import           Text.Hastache.Context

template :: Text -> IO Text
template t = do
  u <- fmap toString nextRandom
  fmap toStrict (hastacheStr defaultConfig t (mkStrContext $ c u))
  where c r "random" = MuVariable r
