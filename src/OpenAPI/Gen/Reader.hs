module OpenAPI.Gen.Reader where
import Control.Algebra
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Yaml as Yaml
import OpenAPI.Support
import OpenAPI.Types
import Network.HTTP.Client
import Network.HTTP.Types.URI
import System.FilePath
import URI.ByteString

readSpec :: IO (Value, Root Unresolved)
readSpec = loadRoot "openapi/openapi/spec3.sdk.json"

loadRoot :: FromJSON a => FilePath -> IO (Value, a)
loadRoot fp = do
  let ext = takeExtension fp
  case ext of
    ".json" -> do
      eVal <- eitherDecodeFileStrict' fp
      let result = do
            val <- eVal
            r <- parseEither parseJSON val
            pure (val, r)
      either fail pure result
    ".yaml" -> do
      eVal <- Yaml.decodeFileThrow fp
      let result = do
            val <- eVal
            r <- parseEither parseJSON val
            pure (val, r)
      either fail pure result
    _ -> error "Unsupported file type"

-- TODO could probably handle non 2xx status codes
loadRemote :: (Has Http sig m) => URIRef Absolute -> m (Either String Value)
loadRemote ref = do
  (HttpResponse r) <- sendRequest $ HttpRequest $ maybeAddAuth $ defaultRequest
    { secure = schemeBS (uriScheme ref) == "https"
    , host = case uriAuthority ref of
        Nothing -> "localhost"
        Just h -> hostBS $ authorityHost h
    , port = case uriAuthority ref >>= authorityPort of
        Nothing -> if schemeBS (uriScheme ref) == "https" then 443 else 80
        Just p -> portNumber p
    -- , authority =
    , path = uriPath ref
    , queryString = renderSimpleQuery True $ queryPairs $ uriQuery ref
    }
  pure $ eitherDecode (responseBody r)
  where
    maybeAddAuth = case uriAuthority ref >>= authorityUserInfo of
      Nothing -> id
      Just u -> applyBasicAuth (uiUsername u) (uiPassword u)
