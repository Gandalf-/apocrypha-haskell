module Apocrypha.Server.Plugin where

import           Apocrypha.Internal.Database (Query)
import           Data.Aeson
import           Data.Text                   (Text)

class Plugin a where
    namespace :: a -> String
    execute   :: a -> Value -> Query -> IO (Text, Bool, Value)
