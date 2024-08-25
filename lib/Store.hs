module Store where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)


type Location = String

-- type Store = HashMap Location Int
type Store a = HashMap Location a


lookup :: Store a -> Location -> Maybe a
lookup = flip HashMap.lookup 


update :: Store Int  -> Location -> Int -> Maybe (Store Int)
update store location value = if HashMap.member location store 
  then Just $ HashMap.insert location value store
  else Nothing  

