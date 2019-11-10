module OpenAPI.Gen.Coders.Xml
  ( httpEncodeXml
  , httpDecodeXml
  ) where

import Control.Algebra
import Control.Effect.Error
import Data.ByteString (ByteString)
import OpenAPI.Support
import Xmlbf

httpEncodeXml :: ToXml a => a -> ByteString
httpEncodeXml = undefined

httpDecodeXml ::
     ( Has (Error (HttpResponseError resp)) sig m
     , FromXml resp
     )
  => HttpResponse
  -> m resp
httpDecodeXml = undefined
