module JSON where 

import Data.Aeson
import Data.Text
import Abs
import Interpreter


instance ToJSON Phrase where 
    toJSON = String . pack . show 


instance ToJSON Context where 
    toJSON = String . pack . show