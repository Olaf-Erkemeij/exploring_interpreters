module JSON where

import Abs
import Data.Aeson
import Data.Text
import Interpreter

instance ToJSON Phrase where
  toJSON = String . pack . show

instance ToJSON Context where
  toJSON = String . pack . show
