module Language.Explorer.Tools.Diff where

import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, Result, Value, encode, decodeStrict')
import Data.Aeson.Diff (diff, patch, Patch, Operation, Pointer, Key)
import Control.DeepSeq (NFData, rnf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.Zstd as Zstd

instance NFData Patch
instance NFData Operation
instance NFData Pointer
instance NFData Key

defaultCompressionLevel :: Int
defaultCompressionLevel = 3

computeDiff  :: (ToJSON a, FromJSON a) => a -> a -> Patch
computeDiff a b = diff (toJSON a) (toJSON b)

patchObject :: (ToJSON a, FromJSON a) => a -> Patch -> Result a
patchObject a p = patch p (toJSON a) >>= fromJSON

compress :: B.ByteString -> B.ByteString
compress = Zstd.compress defaultCompressionLevel

decompress :: B.ByteString -> Maybe B.ByteString
decompress bs =
    case Zstd.decompress bs of
        Zstd.Decompress res -> Just res
        _ -> Nothing

encodeJSON :: ToJSON a => a -> B.ByteString
encodeJSON = BL.toStrict . encode

decodeJSON :: FromJSON a => B.ByteString -> Maybe a
decodeJSON = decodeStrict'
