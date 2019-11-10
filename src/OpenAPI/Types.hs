{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module OpenAPI.Types where
import Control.Applicative hiding (optional)
import qualified Control.Applicative as App
import Control.Lens hiding ((.=))
import Control.Lens.Plated
import Control.Lens.TH
import Control.Monad.State.Strict
import Data.Aeson hiding (Encoding)
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Pointer
import Data.Aeson.Types hiding (Encoding)
import qualified Data.Attoparsec.Text as Parse
import qualified Data.ByteString.Char8 as C
import Data.Functor.Classes
import Data.Coerce
import Data.Char (digitToInt)
import Data.Hashable
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as M
import Data.String
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Debug.Trace
import qualified Text.EDE as EDE
import qualified Text.EDE.Filters as EDE
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Network.HTTP.Media as Media
import URI.ByteString
import URI.ByteString.Aeson ()
import Data.Eq.Deriving
import Text.Show.Deriving
import OpenAPI.JsonUtils

liftShowR :: (Show1 f, Show1 r, Show a) => Int -> f (r a) -> ShowS
liftShowR = liftShowsPrec showsPrec1 (liftShowList showsPrec showList)

newtype PathTemplate = PathTemplate { pathSegments :: [PathSegment] }
  deriving (Eq, Ord, Hashable, Generic)

instance Show PathTemplate where
  show = show . mconcat . map (T.unpack . prettySegment) . pathSegments

instance IsString PathTemplate where
  fromString s = case parsePath $ T.pack s of
    Left e -> error e
    Right ok -> ok

data PathSegment
  = ConstSegment Text
  | NamedSegment Text
  deriving (Show, Eq, Generic, Ord)

instance Hashable PathSegment
instance ToJSONKey PathTemplate where
  toJSONKey = ToJSONKeyText
    (mconcat . map prettySegment . pathSegments)
    (E.text . mconcat . map prettySegment . pathSegments)
instance FromJSONKey PathTemplate where
  fromJSONKey = FromJSONKeyTextParser (either fail pure . parsePath)

prettySegment :: PathSegment -> Text
prettySegment (ConstSegment t) = "/" <> t
prettySegment (NamedSegment t) = "/{" <> t <> "}"

rawSegment :: PathSegment -> Text
rawSegment (ConstSegment t) = t
rawSegment (NamedSegment t) = t

parsePath :: Text -> Either String PathTemplate
parsePath = Parse.parseOnly $ fmap PathTemplate $ many (Parse.char '/' *> (parseNamed <|> parseConst))
  where
    parseConst = ConstSegment <$> Parse.takeWhile (/= '/')
    parseNamed = NamedSegment <$> (Parse.char '{' *> Parse.takeWhile1 (/= '}') <* Parse.char '}')

instance ToJSON PathTemplate where
  toJSON = toJSON . mconcat . map prettySegment . pathSegments
  toEncoding = E.text . mconcat . map prettySegment . pathSegments

instance FromJSON PathTemplate where
  parseJSON = withText "Path" $ \t -> case parsePath t of
    Left err -> fail err
    Right ok -> pure ok

newtype Show1Direct = Show1Direct String
instance Show Show1Direct where
  showsPrec n (Show1Direct d) = showParen (n >= 11) (showString d)

-- directShow :: (Show a, Show1 f)
-- directShow s = Show1Direct $ showsPrec1 0 s ""

newtype CommonMark = CommonMark { fromCommonMark :: Text }
  deriving stock (Show, Eq)
  deriving newtype (ToJSON, FromJSON)


type Path = PathTemplate
type Method = Text

data ServerVariable = ServerVariable
  { serverVariableEnum :: Vector Text
  , serverVariableDefault_ :: Text
  , serverVariableDescription :: Maybe CommonMark
  , serverVariableExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance ToJSON ServerVariable where
  toJSON ServerVariable{..} = Object (serverVariableExtensions <> H.fromList
    [ "enum" .= serverVariableEnum
    , "default" .= serverVariableDefault_
    , "description" .= serverVariableDescription
    ])

instance FromJSON ServerVariable where
  parseJSON = withObject "ServerVariable" $ killer $ do
    serverVariableEnum <- fromMaybe mempty <$> optional "enum"
    serverVariableDefault_ <- require "default"
    serverVariableDescription <- optional "description"
    serverVariableExtensions <- takeExtensions
    pure ServerVariable{..}

data Server = Server
  { serverUrl :: Text
  , serverDescription :: Maybe CommonMark
  , serverVariables :: H.HashMap Text ServerVariable
  , serverExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance ToJSON Server where
  toJSON Server{..} = Object (serverExtensions <> H.fromList
    [ "url" .= serverUrl
    , "description" .= serverDescription
    , "variables" .= serverVariables
    ])

instance FromJSON Server where
  parseJSON = withObject "Server" $ killer $ do
    serverUrl <- require "url"
    serverDescription <- optional "description"
    serverVariables <- fromMaybe H.empty <$> optional "variables"
    serverExtensions <- takeExtensions
    pure Server{..}

type Extensions = H.HashMap Text Value
type Expression = Text

data Link = Link
  { linkOperationRef :: Maybe Text
  , linkOperationId :: Maybe Text
  , linkParameters :: H.HashMap Text Value
  , linkRequestBody :: Maybe Value
  , linkDescription :: Maybe CommonMark
  , linkServer :: Maybe Server
  , linkExtensions :: Extensions
  } deriving (Show, Eq)

instance ToJSON Link where
  toJSON Link{..} = Object (linkExtensions <> H.fromList
    [ "operationRef" .= linkOperationRef
    , "operationId" .= linkOperationId
    , "parameters" .= linkParameters
    , "requestBody" .= linkRequestBody
    , "description" .= linkDescription
    , "server" .= linkServer
    ])

instance FromJSON Link where
  parseJSON = withObject "Link" $ killer $ do
    linkOperationRef <- optional "operationRef"
    linkOperationId <- optional "operationId"
    linkParameters <- defaultOption H.empty "parameters"
    linkRequestBody <- optional "requestBody"
    linkDescription <- optional "description"
    linkServer <- optional "server"
    linkExtensions <- takeExtensions
    pure Link{..}

data Example = Example
  { exampleSummary :: Maybe Text
  , exampleDescription :: Maybe CommonMark
  , exampleValue :: Maybe Value
  , exampleExternalValue :: Maybe Text
  , exampleExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance ToJSON Example where
  toJSON Example{..} = Object (exampleExtensions <> H.fromList
    [ "summary" .= exampleSummary
    , "description" .= exampleDescription
    , "value" .= exampleValue
    , "externalValue" .= exampleExternalValue
    ])

instance FromJSON Example where
  parseJSON = withObject "Example" $ killer $ do
    exampleSummary <- optional "summary"
    exampleDescription <- optional "description"
    exampleValue <- optional "value"
    exampleExternalValue <- optional "externalValue"
    exampleExtensions <- takeExtensions
    pure Example{..}

newtype MT = MT { unMT :: Media.MediaType }
  deriving (Eq, Ord, Show, IsString, Media.Accept, Media.RenderHeader)

instance FromJSON MT where
  parseJSON = withText "MediaType" $ \t ->
    case Media.parseAccept (T.encodeUtf8 t) of
      Nothing -> fail "Invalid MediaType"
      Just mt -> pure $ MT mt

instance ToJSON MT where
  toJSON = toJSON . T.decodeUtf8 . Media.renderHeader

instance FromJSONKey MT where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case Media.parseAccept (T.encodeUtf8 t) of
      Nothing -> fail "Invalid MediaType"
      Just mt -> pure $ MT mt

instance ToJSONKey MT where
  toJSONKey = toJSONKeyText (T.decodeUtf8 . Media.renderHeader)

type Header = Parameter

data Response resolution = Response
  { responseContent :: M.Map MT (resolution (MediaType resolution))
  , responseDescription :: CommonMark
  , responseHeaders :: RefMap resolution (Header resolution)
  , responseLinks :: RefMap resolution Link
  }

instance (Show1 r) => Show (Response r) where
  showsPrec n Response{..} = showParen (n >= 11) $
    showString "Response {" .
    showString "responseContent = " .
    liftShowR 0 responseContent .
    showString ", responseDescription = " .
    showsPrec 0 responseDescription .
    showString ", responseHeaders = " .
    liftShowR 0 responseHeaders .
    showString ", responseLinks = " .
    liftShowR 0 responseLinks .
    showString "}"


instance (Eq1 r) => Eq (Response r) where
  l == r =
    (liftEq eq1 (responseContent l) (responseContent r)) &&
    (responseDescription l == responseDescription r) &&
    (liftEq eq1 (responseHeaders l) (responseHeaders r)) &&
    (liftEq eq1 (responseLinks l) (responseLinks r))

instance ToJSON1 r => ToJSON (Response r) where
  toJSON Response{..} = object
    [ "content" .= fmap toJSON1 responseContent
    , "description" .= responseDescription
    , "headers" .= fmap toJSON1 responseHeaders
    , "links" .= fmap toJSON1 responseLinks
    ]

instance (FromJSON1 r, FromJSON (Schema r)) => FromJSON (Response r) where
  parseJSON = withObject "Response" $ killer $ do
    responseDescription <- require "description"
    responseContent <- (defaultOption1 mempty "content" >>= lift . traverse parseJSON1)
    responseHeaders <- optRefMap "headers"
    responseLinks <- optRefMap "links"
    pure Response{..}

type Callback = Object

data SecuritySchemeType
  = SecuritySchemeAPIKey
  | SecuritySchemeHTTP
  | SecuritySchemeOAuth2
  | SecuritySchemeOpenIDConnect
  deriving (Show, Eq)

instance FromJSON SecuritySchemeType where
  parseJSON = withText "SecuritySchemeType" $ \case
    "apiKey" -> pure SecuritySchemeAPIKey
    "http" -> pure SecuritySchemeHTTP
    "oauth2" -> pure SecuritySchemeOAuth2
    "openIdConnect" -> pure SecuritySchemeOpenIDConnect
    str -> fail ("Invalid Security Scheme type: " <> show str)

data APIKeyLocation
  = APIKeyLocationQuery
  | APIKeyLocationHeader
  | APIKeyLocationCookie
  deriving (Show, Eq)

instance FromJSON APIKeyLocation where
  parseJSON = withText "APIKeyLocation" $ \case
    "query" -> pure APIKeyLocationQuery
    "header" -> pure APIKeyLocationHeader
    "cookie" -> pure APIKeyLocationCookie
    str -> fail ("Invalid API Key location: " <> show str)

type OAuthFlows = Object

data SecurityScheme = SecurityScheme
  { securitySchemeType_ :: SecuritySchemeType
  , securitySchemeDescription :: Maybe CommonMark
  , securitySchemeName :: Maybe Text
  , securitySchemeIn_ :: Maybe APIKeyLocation
  , securitySchemeScheme :: Maybe Text
  , securitySchemeBearerFormat :: Maybe Text
  , securitySchemeFlows :: Maybe OAuthFlows
  , securitySchemeOpenIdConnectUrl :: Maybe Text
  , securitySchemeExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance FromJSON SecurityScheme where
  parseJSON = withObject "SecurityScheme" $ killer $ do
    securitySchemeType_ <- require "type"
    securitySchemeDescription <- optional "description"
    securitySchemeName <- optional "name"
    securitySchemeIn_ <- optional "in"
    securitySchemeScheme <- optional "scheme"
    securitySchemeBearerFormat <- optional "bearerFormat"
    securitySchemeFlows <- optional "flows"
    securitySchemeOpenIdConnectUrl <- optional "openIdConnectUrl"
    securitySchemeExtensions <- takeExtensions
    pure SecurityScheme{..}

data ParameterIn
  = ParameterInQuery
  | ParameterInHeader
  | ParameterInPath
  | ParameterInCookie
  deriving (Show, Eq)

instance ToJSON ParameterIn where
  toJSON = \case
    ParameterInQuery -> "query"
    ParameterInHeader -> "header"
    ParameterInPath -> "path"
    ParameterInCookie -> "cookie"

instance FromJSON ParameterIn where
  parseJSON = withText "ParameterIn" $ \case
    "query" -> pure ParameterInQuery
    "header" -> pure ParameterInHeader
    "path" -> pure ParameterInPath
    "cookie" -> pure ParameterInCookie
    str -> fail (show str <> " is not a valid value for the \"in\" parameter")

data ParameterStyle
  = ParameterStyleMatrix
  | ParameterStyleLabel
  | ParameterStyleForm
  | ParameterStyleSimple
  | ParameterStyleSpaceDelimited
  | ParameterStylePipeDelimited
  | ParameterStyleDeepObject
  deriving (Show, Eq)

instance ToJSON ParameterStyle where
  toJSON = \case
    ParameterStyleMatrix -> "matrix"
    ParameterStyleLabel -> "label"
    ParameterStyleForm -> "form"
    ParameterStyleSimple -> "simple"
    ParameterStyleSpaceDelimited -> "spaceDelimited"
    ParameterStylePipeDelimited -> "pipeDelimited"
    ParameterStyleDeepObject -> "deepObject"

instance FromJSON ParameterStyle where
  parseJSON = withText "ParameterStyle" $ \case
    "matrix" -> pure ParameterStyleMatrix
    "label" -> pure ParameterStyleLabel
    "form" -> pure ParameterStyleForm
    "simple" -> pure ParameterStyleSimple
    "spaceDelimited" -> pure ParameterStyleSpaceDelimited
    "pipeDelimited" -> pure ParameterStylePipeDelimited
    "deepObject" -> pure ParameterStyleDeepObject
    str -> fail (show str <> " is not a valid value for the \"style\" parameter")

data Parameter resolution = Parameter
  { parameterName :: Text
  , parameterIn_ :: ParameterIn
  , parameterDescription :: Maybe CommonMark
  , parameterRequired :: Bool
  , parameterDeprecated :: Bool
  , parameterAllowEmptyValue :: Bool
  -- Can do schema and style, or content to increase flexibility
  , parameterStyle :: Maybe ParameterStyle
  , parameterExplode :: Bool
  , parameterAllowReserved :: Bool
  , parameterSchema :: Maybe (resolution (Schema resolution))
  , parameterExample :: Maybe Value
  , parameterExamples :: RefMap resolution Example
  , parameterContent :: H.HashMap Text (MediaType resolution)
  , parameterExtensions :: H.HashMap Text Value
  }

instance Eq1 r => Eq (Parameter r) where
  l == r =
    (parameterName l == parameterName r) &&
    (parameterIn_ l == parameterIn_ r) &&
    (parameterDescription l == parameterDescription r) &&
    (parameterRequired l == parameterRequired r) &&
    (parameterDeprecated l == parameterDeprecated r) &&
    (parameterAllowEmptyValue l == parameterAllowEmptyValue r) &&
    (parameterStyle l == parameterStyle r) &&
    (parameterExplode l == parameterExplode r) &&
    (parameterAllowReserved l == parameterAllowReserved r) &&
    (liftEq eq1 (parameterSchema l) (parameterSchema r)) &&
    (parameterExample l == parameterExample r) &&
    (liftEq eq1 (parameterExamples l) (parameterExamples r)) &&
    (parameterContent l == parameterContent r) &&
    (parameterExtensions l == parameterExtensions r)

instance (Show1 f) => Show (Parameter f) where
  showsPrec a Parameter{..} = showParen (a >= 11) $
    showString "Parameter {" .
    showString "parameterName = " .
    showsPrec 0 parameterName .
    showString "," .
    showString "parameterIn_ = " .
    showsPrec 0 parameterIn_ .
    showString "," .
    showString "parameterDescription = " .
    showsPrec 0 parameterDescription .
    showString "," .
    showString "parameterRequired = " .
    showsPrec 0 parameterRequired .
    showString "," .
    showString "parameterDeprecated = " .
    showsPrec 0 parameterDeprecated .
    showString "," .
    showString "parameterAllowEmptyValue = " .
    showsPrec 0 parameterAllowEmptyValue .
    showString "," .
    showString "parameterStyle = " .
    showsPrec 0 parameterStyle .
    showString "," .
    showString "parameterExplode = " .
    showsPrec 0 parameterExplode .
    showString "," .
    showString "parameterAllowReserved = " .
    showsPrec 0 parameterAllowReserved .
    showString "," .
    showString "parameterSchema = " .
    liftShowR 0 parameterSchema .
    showString "," .
    showString "parameterExample = " .
    showsPrec 0 parameterExample .
    showString "," .
    showString "parameterExamples = " .
    liftShowR 0 parameterExamples .
    showString "," .
    showString "parameterContent = " .
    showsPrec1 0 parameterContent .
    showString "," .
    showString "parameterExtensions = " .
    showsPrec 0 parameterExtensions .
    showString "}"

instance ToJSON1 r => ToJSON (Parameter r) where
  toJSON Parameter{..} = object
    [ "name" .= parameterName
    , "in" .= parameterIn_
    , "description" .= parameterDescription
    , "required" .= parameterRequired
    , "deprecated" .= parameterDeprecated
    , "allowEmptyValue" .= parameterAllowEmptyValue
    , "style" .= parameterStyle
    , "explode" .= parameterExplode
    , "allowReserved" .= parameterAllowReserved
    , "schema" .= fmap toJSON1 parameterSchema
    , "example" .= parameterExample
    , "examples" .= fmap toJSON1 parameterExamples
    , "content" .= parameterContent
    ]

instance (FromJSON1 r, FromJSON (Schema r)) => FromJSON (Parameter r) where
  parseJSON = withObject "Parameter" $ killer $ do
    parameterName <- require "name"
    parameterIn_ <- require "in"
    parameterDescription <- optional "description"
    parameterRequired' <- optional "required"
    parameterRequired <- case parameterIn_ of
      ParameterInPath -> if (parameterRequired' == Just True)
        then pure True
        else fail "{\"in\": \"path\"} parameters MUST specify \"required\": true."
      _ -> pure $ fromMaybe False parameterRequired'
    parameterDeprecated <- defaultOption False "deprecated"
    parameterAllowEmptyValue <- defaultOption False "allowEmptyValue"
    parameterStyle <- optional "style"
    let explodeDefault = case parameterStyle of
          Just ParameterStyleForm -> True
          _ -> False
    parameterExplode <- defaultOption explodeDefault "explode"
    parameterAllowReserved <- defaultOption False "allowReserved"
    parameterSchema <- optional1 "schema"
    parameterExample <- optional "example"
    parameterExamples <- optRefMap "examples"
    parameterContent <- defaultOption H.empty "content"
    parameterExtensions <- takeExtensions
    pure Parameter{..}

data Encoding resolution = Encoding
  { encodingContentType :: Maybe Text
  , encodingHeaders :: RefMap resolution (Header resolution)
  , encodingStyle :: Maybe ParameterStyle
  , encodingExplode :: Maybe Bool
  , encodingAllowReserved :: Maybe Bool
  , encodingExtensions :: H.HashMap Text Value
  }

instance (Show1 r) => Show (Encoding r) where
  showsPrec n Encoding{..} = showParen (n >= 11) $
    showString "Encoding {" .
    showString "encodingContentType = " .
    showsPrec 0 encodingContentType .
    showString ", encodingHeaders = " .
    liftShowR 0 encodingHeaders .
    showString ", encodingStyle = " .
    showsPrec 0 encodingStyle .
    showString ", encodingExplode = " .
    showsPrec 0 encodingExplode .
    showString ", encodingAllowReserved = " .
    showsPrec 0 encodingAllowReserved .
    showString ", encodingExtensions = " .
    showsPrec 0 encodingExtensions .
    showString "}"

instance (Eq1 r) => Eq (Encoding r) where
  l == r =
    (encodingContentType l == encodingContentType r) &&
    (liftEq eq1 (encodingHeaders l) (encodingHeaders r)) &&
    (encodingStyle l == encodingStyle r) &&
    (encodingExplode l == encodingExplode r) &&
    (encodingAllowReserved l == encodingAllowReserved r) &&
    (encodingExtensions l == encodingExtensions r)

instance ToJSON1 r => ToJSON (Encoding r) where
  toJSON Encoding{..} = object
    [ "contentType" .= encodingContentType
    , "headers" .= fmap toJSON1 encodingHeaders
    , "style" .= encodingStyle
    , "explode" .= encodingExplode
    , "allowReserved" .= encodingAllowReserved
    ]

instance (FromJSON1 r, FromJSON (Schema r)) => FromJSON (Encoding r) where
  parseJSON = withObject "Encoding" $ killer $ do
    encodingContentType <- optional "contentType"
    encodingHeaders <- optRefMap "headers"
    encodingStyle <- optional "style"
    encodingExplode <- optional "explode"
    encodingAllowReserved <- optional "allowReserved"
    encodingExtensions <- takeExtensions
    pure Encoding{..}

data MediaType resolution = MediaType
  { mediaTypeSchema :: Maybe (resolution (Schema resolution))
  , mediaTypeExample :: Maybe Value
  , mediaTypeExamples :: RefMap resolution Example
  , mediaTypeEncoding :: H.HashMap Text (Encoding resolution)
  }

instance (Show1 r) => Show (MediaType r) where
  showsPrec n MediaType{..} = showParen (n >= 11) $
    showString "MediaType {" .
    showString "mediaTypeSchema = " .
    liftShowR 0 mediaTypeSchema .
    showString ", mediaTypeExample = " .
    showsPrec 0 mediaTypeExample .
    showString ", mediaTypeExamples = " .
    liftShowR 0 mediaTypeExamples .
    showString ", mediaTypeEncoding = " .
    showsPrec 0 mediaTypeEncoding .
    showString "}"


instance (Eq1 r) => Eq (MediaType r) where
  l == r =
    (liftEq eq1 (mediaTypeSchema l) (mediaTypeSchema r)) &&
    (mediaTypeExample l == mediaTypeExample r) &&
    (liftEq eq1 (mediaTypeExamples l) (mediaTypeExamples r)) &&
    (liftEq (==) (mediaTypeEncoding l) (mediaTypeEncoding r))

instance ToJSON1 r => ToJSON (MediaType r) where
  toJSON MediaType{..} = object
    [ "schema" .= fmap toJSON1 mediaTypeSchema
    , "example" .= mediaTypeExample
    , "examples" .= fmap toJSON1 mediaTypeExamples
    , "encoding" .= mediaTypeEncoding
    ]

instance (FromJSON (Schema r), FromJSON1 r) => FromJSON (MediaType r) where
  parseJSON = withObject "MediaType" $ killer $ do
    mediaTypeSchema <- optional1 "schema"
    mediaTypeExample <- optional "example"
    mediaTypeExamples <- optRefMap "examples"
    mediaTypeEncoding <- defaultOption H.empty "encoding"
    pure MediaType{..}

data RequestBody resolution = RequestBody
  { requestBodyContent :: H.HashMap Text (MediaType resolution)
  , requestBodyRequired :: Bool
  }

deriving instance Show1 r => Show (RequestBody r)
deriving instance Eq1 r => Eq (RequestBody r)

instance ToJSON1 r => ToJSON (RequestBody r) where
  toJSON RequestBody{..} = object
    [ "content" .= requestBodyContent
    , "required" .= requestBodyRequired
    ]

instance (FromJSON (Schema r), FromJSON1 r) => FromJSON (RequestBody r) where
  parseJSON = withObject "RequestBody" $ killer $
    RequestBody <$> require "content" <*> require "required"


data Components resolution = Components
  { componentsSchemas :: RefMap resolution (Schema resolution)
  , componentsResponses :: RefMap resolution (Response resolution)
  , componentsParameters :: RefMap resolution (Parameter resolution)
  , componentsExamples :: RefMap resolution Example
  , componentsRequestBodies :: RefMap resolution (RequestBody resolution)
  , componentsHeaders :: RefMap resolution (Header resolution)
  , componentsSecuritySchemes :: RefMap resolution SecurityScheme
  , componentsLinks :: RefMap resolution Link
  , componentsCallbacks :: RefMap resolution Callback
  }

instance (Show1 r) => Show (Components r) where
  showsPrec n Components{..} = showParen (n >= 11) $
    showString "Components {" .
    showString "componentsSchemas = " .
    liftShowR 0 componentsSchemas .
    showString ", componentsResponses = " .
    liftShowR 0 componentsResponses .
    showString ", componentsParameters = " .
    liftShowR 0 componentsParameters .
    showString ", componentsExamples = " .
    liftShowR 0 componentsExamples .
    showString ", componentsRequestBodies = " .
    liftShowR 0 componentsRequestBodies .
    showString ", componentsHeaders = " .
    liftShowR 0 componentsHeaders .
    showString ", componentsSecuritySchemes = " .
    liftShowR 0 componentsSecuritySchemes .
    showString ", componentsLinks = " .
    liftShowR 0 componentsLinks .
    showString ", componentsCallbacks = " .
    liftShowR 0 componentsCallbacks .
    showString "}"

instance (FromJSON1 f, FromJSON (Schema f)) => FromJSON (Components f) where
  parseJSON = withObject "Components" $ killer $ do
    componentsSchemas <- optRefMap "schemas"
    componentsResponses <- optRefMap "responses"
    componentsParameters <- optRefMap "parameters"
    componentsExamples <- optRefMap "examples"
    componentsRequestBodies <- optRefMap "requestBodies"
    componentsHeaders <- optRefMap "headers"
    componentsSecuritySchemes <- optRefMap "securitySchemes"
    componentsLinks <- optRefMap "links"
    componentsCallbacks <- optRefMap "callbacks"
    pure Components{..}

instance Eq1 r => Eq (Components r) where
  l == r =
    (liftEq eq1 (componentsSchemas l) (componentsSchemas r)) &&
    (liftEq eq1 (componentsResponses l) (componentsResponses r)) &&
    (liftEq eq1 (componentsParameters l) (componentsParameters r)) &&
    (liftEq eq1 (componentsExamples l) (componentsExamples r)) &&
    (liftEq eq1 (componentsRequestBodies l) (componentsRequestBodies r)) &&
    (liftEq eq1 (componentsHeaders l) (componentsHeaders r)) &&
    (liftEq eq1 (componentsSecuritySchemes l) (componentsSecuritySchemes r)) &&
    (liftEq eq1 (componentsLinks l) (componentsLinks r)) &&
    (liftEq eq1 (componentsCallbacks l) (componentsCallbacks r))

data Info = Info
  { infoTitle :: Text
  , infoDescription :: Maybe CommonMark
  , infoTermsOfService :: Maybe Text
  , infoContact :: Maybe Contact
  , infoLicense :: Maybe License
  , infoVersion :: Text
  , infoExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance FromJSON Info where
  parseJSON = withObject "Info" $ killer $ do
    infoTitle <- require "title"
    infoDescription <- optional "description"
    infoTermsOfService <- optional "termsOfService"
    infoContact <- optional "contact"
    infoLicense <- optional "license"
    infoVersion <- require "version"
    infoExtensions <- takeExtensions
    pure $ Info{..}

instance ToJSON Info where
  toJSON Info{..} = Object (infoExtensions <> H.fromList
    [ "title" .= infoTitle
    , "description" .= infoDescription
    , "termsOfService" .= infoTermsOfService
    , "contact" .= infoContact
    , "license" .= infoLicense
    , "version" .= infoVersion
    ])

data Root resolution = Root
  { rootOpenapi :: Text
  , rootComponents :: Components resolution
  , rootInfo :: Info
  , rootPaths :: H.HashMap Path (H.HashMap Method (ApiEndpoint resolution))
  , rootSecurity :: Vector Object
  , rootServers :: Vector Server
  , rootTags :: Vector Object
  , rootExternalDocs :: Maybe Object
  }

deriving instance Show (Root Unresolved)
deriving instance Show (Root Resolved)

instance ToJSON1 r => ToJSON (Root r) where
  toJSON Root{..} = object
    [ "openapi" .= rootOpenapi
    , "info" .= rootInfo
    , "paths" .= rootPaths
    , "security" .= rootSecurity
    , "servers" .= rootServers
    , "tags" .= rootTags
    , "externalDocs" .= rootExternalDocs
    ]

instance (FromJSON (Schema r), FromJSON1 r) => FromJSON (Root r) where
  parseJSON = withObject "Root" $ killer $ do
    rootOpenapi <- require "openapi"
    rootInfo <- require "info"
    rootServers <- fromMaybe V.empty <$> optional "servers"
    rootPaths <- require "paths"
    rootComponents <- require "components"
    rootSecurity <- require "security"
    rootTags <- fromMaybe V.empty <$> optional "tags"
    rootExternalDocs <- optional "externalDocs"
    pure $ Root{..}



data Contact = Contact
  { contactName :: Maybe Text
  , contactUrl :: Maybe Text
  , contactEmail :: Maybe Text
  , contactExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance FromJSON Contact where
  parseJSON = withObject "Contact" $ killer $ do
    contactName <- optional "name"
    contactUrl <- optional "url"
    contactEmail <- optional "email"
    contactExtensions <- takeExtensions
    pure Contact{..}

instance ToJSON Contact where
  toJSON Contact{..} = Object (contactExtensions <> H.fromList
    [ "name" .= contactName
    , "url" .= contactUrl
    , "email" .= contactEmail
    ])

data License = License
  { licenseName :: Text
  , licenseUrl :: Maybe Text
  , licenseExtensions :: H.HashMap Text Value
  } deriving (Show, Eq)

instance FromJSON License where
  parseJSON = withObject "License" $ killer $ do
    licenseName <- require "name"
    licenseUrl <- optional "url"
    licenseExtensions <- takeExtensions
    pure License{..}

instance ToJSON License where
  toJSON License{..} = Object (licenseExtensions <> H.fromList
    [ "name" .= licenseName
    , "url" .= licenseUrl
    ])

data Reference a
  = RelativeReference (URIRef Relative)
  | AbsoluteReference (URIRef Absolute)
  deriving (Show, Eq, Ord, Functor)

reRef :: Reference a -> Reference b
reRef (RelativeReference r) = RelativeReference r
reRef (AbsoluteReference r) = AbsoluteReference r

resolvedReference :: Reference (t Unresolved) -> Reference (t Resolved)
resolvedReference = reRef

instance Eq1 Reference where
  liftEq _ l r = l == reRef r

instance ToJSON (Reference a) where
  toJSON (RelativeReference r) = object [ "$ref" .= r ]
  toJSON (AbsoluteReference r) = object [ "$ref" .= r ]

instance FromJSON (Reference a) where
  parseJSON = withObject "Reference" $ killer $ do
    val <- require "$ref"
    lift ((RelativeReference <$> parseJSON val) <|> (AbsoluteReference <$> parseJSON val))

data Resolved a
  = Resolved (Resolution a)
  | RecursiveReference (Reference a)
  deriving (Functor, Ord)

instance ToJSON a => ToJSON (Resolved a) where
  toJSON = toJSON1

instance (Show a) => Show (Resolved a) where
  showsPrec = showsPrec1

instance Show1 Resolved where
  liftShowsPrec showOne showMany n = \case
    Resolved r -> showParen (n >= 11) $ showString "Resolved " . liftShowsPrec showOne showMany 11 r
    RecursiveReference r -> showParen (n >= 11) $ showString "RecursiveReference " . showsPrec 11 r

instance (Eq a) => Eq (Resolved a) where
  (==) = eq1

instance Eq1 Resolved where
  liftEq eqF (Resolved l) (Resolved r) =
    liftEq (liftEq eqF) (ref l) (ref r) &&
    eqF (resolved l) (resolved r)
  liftEq eqF (RecursiveReference l) (RecursiveReference r) = liftEq eqF l r
  liftEq _ _ _ = False

instance ToJSON1 Resolved where
  liftToJSON toOne toMany (Resolved r) = liftToJSON toOne toMany r
  liftToJSON _ _ (RecursiveReference r) = toJSON r
  liftToEncoding toOne toMany (Resolved r) = liftToEncoding toOne toMany r
  liftToEncoding _ _ (RecursiveReference r) = toEncoding r

instance FromJSON1 Resolved where
  liftParseJSON parseOne parseMany v =
    (Resolved <$> liftParseJSON parseOne parseMany v) <|>
    (RecursiveReference <$> parseJSON v)

instance FromJSON a => FromJSON (Resolved a) where
  parseJSON = parseJSON1

instance Foldable Resolved where
  foldr f z (Resolved r) = foldr f z r
  foldr f z (RecursiveReference r) = z

instance Traversable Resolved where
  traverse f (Resolved r) = Resolved <$> traverse f r
  traverse f (RecursiveReference r) = pure (RecursiveReference $ reRef r)

data Resolution a = Resolution { ref :: Maybe (Reference a), resolved :: a }
  deriving (Functor, Ord)

instance (Show a) => Show (Resolution a) where
  showsPrec = showsPrec1

instance Show1 Resolution where
  liftShowsPrec showOne showMany n Resolution{..} = showParen (n >= 11) $
    showString "Resolution {" .
    showString "ref = " .
    showsPrec 0 ref .
    showString ", resolved = " .
    showOne 0 resolved .
    showString "}"

instance (Eq a) => Eq (Resolution a) where
  (==) = eq1

instance Eq1 Resolution where
  liftEq eqF l r =
    liftEq (liftEq eqF) (ref l) (ref r) &&
    eqF (resolved l) (resolved r)

instance ToJSON a => ToJSON (Resolution a) where
  toJSON = toJSON1

instance ToJSON1 Resolution where
  liftToJSON toOne _ Resolution{..} = case ref of
    Nothing -> toOne resolved
    Just val -> toJSON val
  liftToEncoding toOne _ Resolution{..} = case ref of
    Nothing -> toOne resolved
    Just val -> toEncoding val

instance FromJSON1 Resolution where
  liftParseJSON parseOne _ v = Resolution <$> pure Nothing <*> parseOne v

instance FromJSON a => FromJSON (Resolution a) where
  parseJSON = parseJSON1

instance Foldable Resolution where
  foldr f z (Resolution _ a) = f a z

instance Traversable Resolution where
  traverse f (Resolution mr a) = Resolution (coerce mr) <$> f a

data Unresolved a
  = Ref !(Reference a)
  | Obj a
  deriving (Functor, Ord)

instance (Eq a) => Eq (Unresolved a) where
  (==) = eq1

instance (Show a) => Show (Unresolved a) where
  showsPrec = showsPrec1

instance Show1 Unresolved where
  liftShowsPrec _ _ n (Ref r) = showParen (n >= 11) $ showString "Ref " . showsPrec 11 r
  liftShowsPrec showOne _ n (Obj a) = showParen (n >= 11) $ showString "Obj " . showOne 11 a

instance Eq1 Unresolved where
  liftEq eqF (Ref l) (Ref r) = liftEq eqF l r
  liftEq eqF (Obj l) (Obj r) = eqF l r
  liftEq _ _ _ = False

instance ToJSON1 Unresolved where
  liftToJSON _ _ (Ref r) = toJSON r
  liftToJSON toOne _ (Obj x) = toOne x
  liftToEncoding _ _ (Ref r) = toEncoding r
  liftToEncoding toOne _ (Obj x) = toOne x

instance FromJSON1 Unresolved where
  liftParseJSON parseOne _ v = (Ref <$> parseJSON v) <|> (Obj <$> parseOne v)

instance Foldable Unresolved where
  foldr f z (Ref r) = z
  foldr f z (Obj a) = f a z

instance Traversable Unresolved where
  traverse f (Ref (RelativeReference r)) = pure $ Ref $ RelativeReference r
  traverse f (Ref (AbsoluteReference r)) = pure $ Ref $ AbsoluteReference r
  traverse f (Obj a) = Obj <$> f a

instance ToJSON a => ToJSON (Unresolved a) where
  toJSON = toJSON1

instance FromJSON a => FromJSON (Unresolved a) where
  parseJSON = parseJSON1





{-
data OAuthFlows = OAuthFlows
  { oauthFlowsImplicit :: Maybe ImplicitFlow
  , oauthFlowsPassword :: Maybe PasswordFlow
  , oauthFlowsClientCredentials :: Maybe CredentialsFlow
  , oauthFlowsAuthorizationCode :: Maybe AuthorizationCodeFlow
  } deriving (Show, Eq)

data ImplicitFlow = ImplicitFlow
  { implicitFlowAuthorizationUrl :: Text
  , implicitFlowTokenUrl :: Text
  , implicitFlowRefreshUrl :: Maybe Text
  , implicitFlowScopes :: H.HashMap Text Text
  , implicitFlowExten
  } deriving (Show, Eq)

data PasswordFlow = PasswordFlow
  { passwordFlowTokenUrl :: Text
  , passwordFlowRefreshUrl ::
  }
-}


data Schema resolution = Schema
  { schemaProperties :: RefMap resolution (Schema resolution)
  , schemaTitle :: Maybe Text
  , schemaDescription :: Maybe CommonMark
  , schemaType_ :: Maybe Text
  , schemaRequired :: Vector Text
  , schemaAnyOf :: Vector (resolution (Schema resolution))
  , schemaItems :: Maybe (resolution (Schema resolution)) -- MUST be present if type is array
  , schemaMinLength :: Maybe Word
  , schemaMaxLength :: Maybe Word
  , schemaFormat :: Maybe Text
  , schemaEnum :: Maybe (Vector Value)
  , schemaAdditionalProperties :: Maybe (AdditionalProperties resolution)
  , schemaPattern :: Maybe Text
  , schemaNullable :: Bool
  , schemaReadOnly :: Maybe Bool
  , schemaWriteOnly :: Maybe Bool
  , schemaExtensions :: Extensions
  }

instance Show1 r => Show (Schema r) where
  showsPrec a Schema{..} = showParen (a >= 11) $
    showString "Schema {" .
    showString "schemaProperties = " .
    liftShowR 0 schemaProperties .
    showString ", schemaTitle = " .
    showsPrec 0 schemaTitle .
    showString ", schemaDescription = " .
    showsPrec 0 schemaDescription .
    showString ", schemaType_ = " .
    showsPrec 0 schemaType_ .
    showString ", schemaRequired = " .
    showsPrec 0 schemaRequired .
    showString ", schemaAnyOf = " .
    liftShowR 0 schemaAnyOf .
    showString ", schemaItems = " .
    liftShowR 0 schemaItems .
    showString ", schemaMinLength = " .
    showsPrec 0 schemaMinLength .
    showString ", schemaMaxLength = " .
    showsPrec 0 schemaMaxLength .
    showString ", schemaFormat = " .
    showsPrec 0 schemaFormat .
    showString ", schemaEnum = " .
    showsPrec 0 schemaEnum .
    showString ", schemaAdditionalProperties = " .
    showsPrec 0 schemaAdditionalProperties .
    showString ", schemaPattern = " .
    showsPrec 0 schemaPattern .
    showString ", schemaNullable = " .
    showsPrec 0 schemaNullable .
    showString ", schemaReadOnly = " .
    showsPrec 0 schemaReadOnly .
    showString ", schemaWriteOnly = " .
    showsPrec 0 schemaWriteOnly .
    showString ", schemaExtensions = " .
    showsPrec 0 schemaExtensions .
    showString "}"

instance Eq1 r => Eq (Schema r) where
  l == r =
    (liftEq eq1 (schemaProperties l) (schemaProperties r)) &&
    (schemaTitle l == schemaTitle r) &&
    (schemaDescription l == schemaDescription r) &&
    (schemaType_ l == schemaType_ r) &&
    (schemaRequired l == schemaRequired r) &&
    (liftEq eq1 (schemaAnyOf l) (schemaAnyOf r)) &&
    (liftEq eq1 (schemaItems l) (schemaItems r)) &&
    (schemaMinLength l == schemaMinLength r) &&
    (schemaMaxLength l == schemaMaxLength r) &&
    (schemaFormat l == schemaFormat r) &&
    (schemaEnum l == schemaEnum r) &&
    (schemaAdditionalProperties l == schemaAdditionalProperties r) &&
    (schemaPattern l == schemaPattern r) &&
    (schemaNullable l == schemaNullable r) &&
    (schemaReadOnly l == schemaReadOnly r) &&
    (schemaWriteOnly l == schemaWriteOnly r) &&
    (schemaExtensions l == schemaExtensions r)

emptySchema :: Schema resolution
emptySchema = Schema
  { schemaProperties = mempty
  , schemaTitle = Nothing
  , schemaDescription = Nothing
  , schemaType_ = Nothing
  , schemaRequired = mempty
  , schemaAnyOf = mempty
  , schemaItems = Nothing
  , schemaMinLength = Nothing
  , schemaMaxLength = Nothing
  , schemaFormat = Nothing
  , schemaEnum = Nothing
  , schemaAdditionalProperties = Nothing
  , schemaPattern = Nothing
  , schemaNullable = False
  , schemaReadOnly = Nothing
  , schemaWriteOnly = Nothing
  , schemaExtensions = mempty
  }

-- TODO this needs liberal doses of quickcheck applied.
instance ToJSON1 r => ToJSON (Schema r) where
  toJSON Schema{..} = object (baseFields <> H.toList schemaExtensions)
    where
      baseFields = catMaybes
        [ if H.null schemaProperties
          then Nothing
          else Just ("properties" .= fmap toJSON1 schemaProperties)
        , ("title" .=) <$> schemaTitle
        , ("description" .=) <$> schemaDescription
        , ("type" .=) <$> schemaType_
        , if V.null schemaRequired
          then Nothing
          else Just ("required" .= schemaRequired)
        , if V.null schemaAnyOf
          then Nothing
          else Just ("anyOf" .= fmap toJSON1 schemaAnyOf)
        , (\x -> "items" .= toJSON1 x) <$> schemaItems
        , ("minLength" .=) <$> schemaMinLength
        , ("maxLength" .=) <$> schemaMaxLength
        , ("format" .=) <$> schemaFormat
        , ("enum" .=) <$> schemaEnum
        , ("additionalProperties" .=) <$> schemaAdditionalProperties
        , ("pattern" .=) <$> schemaPattern
        , if schemaNullable
          then Just ("nullable" .= schemaNullable)
          else Nothing
        , ("readOnly" .=) <$> schemaReadOnly
        , ("writeOnly" .=) <$> schemaWriteOnly
        ]

instance (FromJSON1 f, FromJSON (f (Schema f))) => FromJSON (Schema f) where
  parseJSON = withObject "Schema" $ killer $ do
    schemaProperties <- defaultOption H.empty "properties"
    schemaTitle <- optional "title"
    schemaDescription <- optional "description"
    schemaType_ <- optional "type"
    schemaRequired <- defaultOption mempty "required"
    schemaAnyOf <- defaultOption mempty "anyOf"
    schemaItems <- optional "items"
    schemaMinLength <- optional "minLength"
    schemaMaxLength <- optional "maxLength"
    schemaFormat <- optional "format"
    schemaEnum <- optional "enum"
    schemaAdditionalProperties <- optional "additionalProperties"
    schemaPattern <- optional "pattern"
    schemaNullable <- defaultOption False "nullable"
    schemaReadOnly <- optional "readOnly"
    schemaWriteOnly <- optional "writeOnly"
    schemaExtensions <- takeExtensions
    pure Schema{..}

data AdditionalProperties resolution
  = AdditionalPropertiesToggle Bool
  | AdditionalPropertiesSchema (resolution (Schema resolution))

instance (Show1 r) => Show (AdditionalProperties r) where
  showsPrec n (AdditionalPropertiesToggle t) = showParen (n >= 11) $ showString "AdditionalPropertiesToggle " . showsPrec n t
  showsPrec n (AdditionalPropertiesSchema t) = showParen (n >= 11) $ showString "AdditionalPropertiesSchema " . showsPrec1 11 t

instance (Eq1 r, Eq (Schema r)) => Eq (AdditionalProperties r) where
  (AdditionalPropertiesToggle l) == (AdditionalPropertiesToggle r) = l == r
  (AdditionalPropertiesSchema l) == (AdditionalPropertiesSchema r) = eq1 l r

instance ToJSON1 r => ToJSON (AdditionalProperties r) where
  toJSON (AdditionalPropertiesToggle t) = toJSON t
  toJSON (AdditionalPropertiesSchema s) = toJSON1 s

instance (FromJSON (Schema r), FromJSON1 r) =>
         FromJSON (AdditionalProperties r) where
  parseJSON (Bool t) = pure $ AdditionalPropertiesToggle t
  parseJSON other = AdditionalPropertiesSchema <$> parseJSON1 other

data ApiEndpoint resolution = ApiEndpoint
  { apiEndpointDescription :: CommonMark
  , apiEndpointOperationId :: Text
  , apiEndpointParameters :: Vector (Parameter resolution)
  , apiEndpointRequestBody :: RequestBody resolution
  , apiEndpointResponses :: Responses resolution
  , apiEndpointServers :: Vector Server
  , apiEndpointDeprecated :: Bool
  }

deriving instance (Show1 f) => Show (ApiEndpoint f)
deriving instance (Eq1 f) => Eq (ApiEndpoint f)

instance ToJSON1 r => ToJSON (ApiEndpoint r) where
  toJSON ApiEndpoint{..} = object
    [ "description" .= apiEndpointDescription
    , "operationId" .= apiEndpointOperationId
    , "parameters" .= apiEndpointParameters
    , "requestBody" .= apiEndpointRequestBody
    , "responses" .= apiEndpointResponses
    , "servers" .= apiEndpointServers
    , "deprecated" .= apiEndpointDeprecated
    ]

instance (FromJSON (Schema r), FromJSON1 r) => FromJSON (ApiEndpoint r) where
  parseJSON = withObject "ApiEndpoint" $ killer $ do
    apiEndpointDescription <- require "description"
    apiEndpointOperationId <- require "operationId"
    apiEndpointParameters <- (fromMaybe mempty <$> optional "parameters")
    apiEndpointRequestBody <- require "requestBody"
    apiEndpointResponses <- require "responses"
    apiEndpointServers <- (fromMaybe mempty <$> optional "servers")
    apiEndpointDeprecated <- (fromMaybe False <$> optional "deprecated")
    pure ApiEndpoint{..}

data StatusPatternElem
  = Digit Int
  | Wildcard
  deriving (Eq, Generic)

instance Hashable StatusPatternElem

instance Show StatusPatternElem where
  show = \case
    Digit i -> show i
    Wildcard -> "X"

data ResponseKey
  = ConstStatus !Int
  | StatusPatternResponse StatusPatternElem StatusPatternElem StatusPatternElem
  | DefaultResponse
  deriving (Eq, Generic)

instance Hashable ResponseKey

instance Show ResponseKey where
  show = \case
    ConstStatus i -> show i
    StatusPatternResponse h m l -> concatMap show [h, m, l]
    DefaultResponse -> "default"

instance ToJSON ResponseKey where
  toJSON = toJSON . T.pack . show

instance ToJSONKey ResponseKey where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSON ResponseKey where
  parseJSON = withText "Response Key" $ \t -> either fail pure $ parseResponseKey t

instance FromJSONKey ResponseKey where
  fromJSONKey = FromJSONKeyTextParser $ \t -> either fail pure $ parseResponseKey t

instance EDE.Unquote ResponseKey

parseResponseKey :: Text -> Either String ResponseKey
parseResponseKey = Parse.parseOnly parser
  where
    parseChar = fmap (Digit . digitToInt) Parse.digit <|> (Parse.char 'X' *> pure Wildcard)
    parseConstOrPattern = do
      high <- parseChar
      middle <- parseChar
      low <- parseChar
      case (high, middle, low) of
        (Digit h, Digit m, Digit l) -> pure $ ConstStatus $ (h * 100) + (m * 10) + l
        _ -> pure $ StatusPatternResponse high middle low

    parser = (Parse.string "default" *> pure DefaultResponse) <|> parseConstOrPattern

data Responses resolution = Responses
  { responsesResponses :: H.HashMap ResponseKey (Response resolution)
  }

instance (Show1 r) => Show (Responses r) where
  showsPrec n Responses{..} = showParen (n >= 11) $ showString "Responses {responsesResponses = " . showsPrec 0 responsesResponses . showString "}"

instance (Eq1 r) => Eq (Responses r) where
  l == r = liftEq (==) (responsesResponses l) (responsesResponses r)

instance ToJSON1 r => ToJSON (Responses r) where
  toJSON = toJSON . responsesResponses

instance (FromJSON1 r, FromJSON (Schema r)) => FromJSON (Responses r) where
  parseJSON v = Responses <$> parseJSON v

makeFields ''Root
makeFields ''Info
makeFields ''Contact
makeFields ''License
makeFields ''Reference
makePrisms ''Unresolved
makeFields ''Components
makeFields ''Parameter
makeFields ''SecurityScheme
makeFields ''Link
makeFields ''Schema
makeFields ''ApiEndpoint
makeFields ''Server
makeFields ''ServerVariable
makeFields ''Example
makeFields ''RequestBody
makeFields ''Responses
makeFields ''Response
makeFields ''MediaType
makeFields ''Encoding
makePrisms ''Resolved

instance Traversable f => Plated (Schema f) where
  plate f Schema{..} = Schema <$>
    (traverse (traverse f) schemaProperties) <*>
    (pure schemaTitle) <*>
    (pure schemaDescription) <*>
    (pure schemaType_) <*>
    (pure schemaRequired) <*>
    (traverse (traverse f) schemaAnyOf) <*>
    (traverse (traverse f) schemaItems) <*>
    (pure schemaMinLength) <*>
    (pure schemaMaxLength) <*>
    (pure schemaFormat) <*>
    (pure schemaEnum) <*>
    (traverse (\ps -> case ps of
         AdditionalPropertiesToggle _ -> pure ps
         AdditionalPropertiesSchema rs -> AdditionalPropertiesSchema <$> traverse f rs
     ) schemaAdditionalProperties) <*>
    (pure schemaPattern) <*>
    (pure schemaNullable) <*>
    (pure schemaReadOnly) <*>
    (pure schemaWriteOnly) <*>
    (pure schemaExtensions)
