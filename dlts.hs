import Data.List (isPrefixOf)

step l ds
  | "#define DLT_" `isPrefixOf` l = secondWord l : ds
  | otherwise                     = ds
  where secondWord = head . tail . words
