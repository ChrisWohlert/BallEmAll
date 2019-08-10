module Util where

import Data.Maybe

maybeHead :: [Maybe a] -> Maybe a
maybeHead xs =
  case take 1 $ filter isJust xs of
    [] -> Nothing
    [x] -> x

