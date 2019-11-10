{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- Derived from OpenAPI spec
module Stripe.Types.DeletedTerminal where

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



-- Describes: "deleted_terminal.location"

data Location
  = Location
    { locationId :: Id (DeletedTerminalLocation)
    {- ^ Unique identifier for the object. -}
    , locationObject :: DeletedTerminalLocationObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , locationDeleted :: Bool
    {- ^ Always true for a deleted object -}
    } deriving (Show, Eq, Generic)


-- Describes: "deleted_terminal.reader"

data Reader
  = Reader
    { readerId :: Id (DeletedTerminalReader)
    {- ^ Unique identifier for the object. -}
    , readerObject :: DeletedTerminalReaderObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , readerDeleted :: Bool
    {- ^ Always true for a deleted object -}
    } deriving (Show, Eq, Generic)


