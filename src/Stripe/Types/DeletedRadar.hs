{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- Derived from OpenAPI spec
module Stripe.Types.DeletedRadar where

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



-- Describes: "deleted_radar.value_list"

data ValueList
  = ValueList
    { valueListId :: Id (DeletedRadarValueList)
    {- ^ Unique identifier for the object. -}
    , valueListObject :: DeletedRadarValueListObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , valueListDeleted :: Bool
    {- ^ Always true for a deleted object -}
    } deriving (Show, Eq, Generic)


-- Describes: "deleted_radar.value_list_item"

data ValueListItem
  = ValueListItem
    { valueListItemId :: Id (DeletedRadarValueListItem)
    {- ^ Unique identifier for the object. -}
    , valueListItemObject :: DeletedRadarValueListItemObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , valueListItemDeleted :: Bool
    {- ^ Always true for a deleted object -}
    } deriving (Show, Eq, Generic)


