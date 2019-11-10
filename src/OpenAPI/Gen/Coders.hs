module OpenAPI.Gen.Coders where
import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import OpenAPI.Types
{-
builtInContentTypes :: H.HashMap MT ContentTypeHandler
builtInContentTypes = H.fromList
  [ ("application/json", ContentTypeHandler "Json")
  , ("application/octet-stream", ContentTypeHandler "Binary")
  , ("text/xml", ContentTypeHandler "Xml")
  , ("text/html", ContentTypeHandler "Html")
  , ("application/x-www-form-urlencoded", ContentTypeHandler "Form")
  , ("multipart/form-data", ContentTypeHandler "MultiPart")
  ]
-}

newtype Coders = Coders { fromCoders :: M.Map MT Coder }
  deriving (Show, Eq, Semigroup, Monoid, ToJSON)

resolveCoder :: Coders -> MT -> Maybe Coder
resolveCoder (Coders cs) mt = M.lookup mt cs

includeCoders :: Coders -> [MT] -> Coders
includeCoders (Coders c) l = Coders $ M.filterWithKey (\k _ -> k `elem` l) c

data Coder = Coder
  { coderSuffix :: Text
  , coderSupport :: CoderSupport
  } deriving (Show, Eq)

instance ToJSON Coder where
  toJSON Coder{..} = object
    [ "suffix" .= coderSuffix
    , "support" .= coderSupport
    ]

data CoderSupport
  = EncoderOnly Encoder
  | DecoderOnly Decoder
  | BidirectionalCoding Encoder Decoder
  deriving (Show, Eq)

instance ToJSON CoderSupport where
  toJSON = \case
    EncoderOnly enc -> object
      [ "type" .= ("encodeOnly" :: Text)
      , "encoder" .= enc
      ]
    DecoderOnly dec -> object
      [ "type" .= ("decodeOnly" :: Text)
      , "decoder" .= dec
      ]
    BidirectionalCoding enc dec -> object
      [ "type" .= ("bidirectional" :: Text)
      , "encoder" .= enc
      , "decoder" .= dec
      ]

data Decoder = Decoder
  { decoderTemplatePath :: FilePath
  , decoderExtraPackages :: [Text]
  , decoderExtraImports :: [Text]
  , decoderFunction :: Text
  } deriving (Show, Eq)

instance ToJSON Decoder where
  toJSON Decoder{..} = object
    [ "templatePath" .= decoderTemplatePath
    , "extraImports" .= decoderExtraImports
    , "function" .= decoderFunction
    ]

data Encoder = Encoder
  { encoderTemplatePath :: FilePath
  , encoderExtraPackages :: [Text]
  , encoderExtraImports :: [Text]
  , encoderFunction :: Text
  } deriving (Show, Eq)

instance ToJSON Encoder where
  toJSON Encoder{..} = object
    [ "templatePath" .= encoderTemplatePath
    , "extraImports" .= encoderExtraImports
    , "function" .= encoderFunction
    ]

jsonCoder :: Coder
jsonCoder = Coder "Json" $ BidirectionalCoding encoder decoder
  where
    encoder = Encoder
      { encoderTemplatePath = "template/_include/encoder/json.ede"
      , encoderExtraPackages = []
      , encoderExtraImports = ["OpenAPI.Gen.Coders.Json"]
      , encoderFunction = "httpEncodeJson"
      }
    decoder = Decoder
      { decoderTemplatePath = "template/_include/decoder/json.ede"
      , decoderExtraPackages = []
      , decoderExtraImports = ["OpenAPI.Gen.Coders.Json"]
      , decoderFunction = "httpDecodeJson"
      }

formCoder :: Coder
formCoder = Coder "Form" $ BidirectionalCoding encoder decoder
  where
    encoder = Encoder
      { encoderTemplatePath = "template/_include/encoder/form.ede"
      , encoderExtraPackages = []
      , encoderExtraImports = ["OpenAPI.Gen.Coders.Form"]
      , encoderFunction = "httpEncodeForm"
      }
    decoder = Decoder
      { decoderTemplatePath = "template/_include/decoder/form.ede"
      , decoderExtraPackages = []
      , decoderExtraImports = ["OpenAPI.Gen.Coders.Form"]
      , decoderFunction = "httpDecodeForm"
      }

xmlCoder :: Coder
xmlCoder = Coder "Xml" $ BidirectionalCoding encoder decoder
  where
    encoder = Encoder
      { encoderTemplatePath = "template/_include/encoder/xml.ede"
      , encoderExtraPackages = []
      , encoderExtraImports = ["OpenAPI.Gen.Coders.Xml"]
      , encoderFunction = "httpEncodeXml"
      }
    decoder = Decoder
      { decoderTemplatePath = "template/_include/decoder/xml.ede"
      , decoderExtraPackages = []
      , decoderExtraImports = ["OpenAPI.Gen.Coders.Xml"]
      , decoderFunction = "httpDecodeXml"
      }

standardCoders :: Coders
standardCoders = Coders $ M.fromList
  [ ( "application/json", jsonCoder)
  , ( "application/x-www-form-encoded", formCoder)
  , ( "application/xml" , xmlCoder)
  , ( "text/xml", xmlCoder)
  {-
  , ( "text/plain"
    , DecoderOnly $ Decoder
      { decoderTemplatePath = "template/_include/decoder/plaintext.ede"
      , decoderExtraImports = ["OpenAPI.Support.PlainText"]
      , decoderFunction = "httpDecodePlainText"
      }
    )
    -}
  ]
