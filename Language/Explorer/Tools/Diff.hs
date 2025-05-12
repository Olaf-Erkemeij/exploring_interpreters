module Language.Explorer.Tools.Diff where

import qualified Codec.Compression.Zstd as Zstd
import Control.DeepSeq (NFData, rnf)
import Data.Aeson (FromJSON, Result, ToJSON, Value, decodeStrict', encode, fromJSON, toJSON)
import Data.Aeson.Diff (Key, Operation, Patch, Pointer, diff, patch)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

instance NFData Patch

instance NFData Operation

instance NFData Pointer

instance NFData Key

computeDiff :: (ToJSON a, FromJSON a) => a -> a -> Patch
computeDiff a b = diff (toJSON a) (toJSON b)

patchObject :: (ToJSON a, FromJSON a) => a -> Patch -> Result a
patchObject a p = patch p (toJSON a) >>= fromJSON

compress :: Int -> B.ByteString -> B.ByteString
compress = Zstd.compress

decompress :: B.ByteString -> Maybe B.ByteString
decompress bs =
  case Zstd.decompress bs of
    Zstd.Decompress res -> Just res
    _ -> Nothing

encodeJSON :: ToJSON a => a -> B.ByteString
encodeJSON = BL.toStrict . encode

decodeJSON :: FromJSON a => B.ByteString -> Maybe a
decodeJSON = decodeStrict'

encodeBinary :: Binary a => a -> B.ByteString
encodeBinary = BL.toStrict . Binary.encode

decodeBinary :: Binary a => B.ByteString -> Maybe a
decodeBinary bs =
  case Binary.decodeOrFail (BL.fromStrict bs) of
    Left _ -> Nothing
    Right (r, _, result) -> if BL.null r then Just result else Nothing
