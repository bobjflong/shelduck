module Templating where

import           Data.Text
import           Data.Text.Lazy        (toStrict)
import qualified Data.Text.Lazy.IO     as TL
import           System.Random
import           Text.Hastache
import           Text.Hastache.Context

template :: Text -> IO Text
template t = do
  r <- randomIO :: IO Float
  fmap toStrict (hastacheStr defaultConfig t (mkStrContext $ c r))
  where c r "random" = MuVariable $ (pack . show) r
