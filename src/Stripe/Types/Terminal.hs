{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- Derived from OpenAPI spec
module Stripe.Types.Terminal where

-- TODO all of this needs to be qualified to account for generated output causing conflicts
import Control.Applicative ((<|>))
import Data.Aeson (ToJSON1(..), FromJSON1(..), toJSON1, parseJSON1, Value(..))
import Data.Data (Data(..))
import Data.Functor.Const
import qualified Data.Sum
import GHC.Generics (Generic)
import OpenAPI.Support hiding (Error(..))
import OpenAPI.Support.Stripe

-- TODO All objects going into qery string need instance of QueryValueLike based on parameter format



-- Describes: "terminal.connection_token"

data ConnectionToken
  = ConnectionToken
    { connectionTokenObject :: TerminalConnectionTokenObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , connectionTokenLocation :: Text
    {- ^ The id of the location that this connection token is scoped to. -}
    , connectionTokenSecret :: Text
    {- ^ Your application should pass this token to the Stripe Terminal SDK. -}
    } deriving (Show, Eq, Generic)


-- Describes: "terminal.location"

data Location
  = Location
    { locationId :: Id (TerminalLocation)
    {- ^ Unique identifier for the object. -}
    , locationObject :: TerminalLocationObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , locationAddress :: Address
    , locationDisplayName :: Text
    {- ^ The display name of the location. -}
    } deriving (Show, Eq, Generic)


-- Describes: "terminal.reader"

data Reader
  = Reader
    { readerId :: Id (TerminalReader)
    {- ^ Unique identifier for the object. -}
    , readerObject :: TerminalReaderObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , readerDeviceType :: Text
    {- ^ Type of reader, e.g., `verifone_P400` or `bbpos_chipper2x`. -}
    , readerLabel :: Text
    {- ^ Custom label given to the reader for easier identification. -}
    , readerSerialNumber :: Text
    {- ^ Serial number of the reader. -}
    , readerDeviceSwVersion :: Maybe (Text)
    {- ^ The current software version of the reader. -}
    , readerIpAddress :: Maybe (Text)
    {- ^ The local IP address of the reader. -}
    , readerLocation :: Maybe (Text)
    {- ^ The location identifier of the reader. -}
    , readerStatus :: Maybe (Text)
    {- ^ The networking status of the reader. -}
    } deriving (Show, Eq, Generic)


