{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- Derived from OpenAPI spec
module Stripe.Types.Radar where

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



-- Describes: "radar.early_fraud_warning"

data EarlyFraudWarning
  = EarlyFraudWarning
    { earlyFraudWarningId :: Id (RadarEarlyFraudWarning)
    {- ^ Unique identifier for the object. -}
    , earlyFraudWarningObject :: RadarEarlyFraudWarningObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , earlyFraudWarningLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , earlyFraudWarningActionable :: Bool
    {- ^ An EFW is actionable if it has not received a dispute and has not been fully refunded. You may wish to proactively refund a charge that receives an EFW, in order to avoid receiving a dispute later. -}
    , earlyFraudWarningCharge :: Either (Text) (Charge)
    {- ^ ID of the charge this early fraud warning is for, optionally expanded. -}
    , earlyFraudWarningCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , earlyFraudWarningFraudType :: RadarEarlyFraudWarningFraudTypeEnum
    {- ^ The type of fraud labelled by the issuer. One of `card_never_received`, `fraudulent_card_application`, `made_with_counterfeit_card`, `made_with_lost_card`, `made_with_stolen_card`, `misc`, `unauthorized_use_of_card`. -}
    } deriving (Show, Eq, Generic)


-- Describes: "radar.value_list"

data ValueList
  = ValueList
    { valueListId :: Id (RadarValueList)
    {- ^ Unique identifier for the object. -}
    , valueListObject :: RadarValueListObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , valueListLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , valueListAlias :: Text
    {- ^ The name of the value list for use in rules. -}
    , valueListCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , valueListCreatedBy :: Text
    {- ^ The name or email address of the user who created this value list. -}
    , valueListItemType :: RadarValueListItemTypeEnum
    {- ^ The type of items in the value list. One of `card_fingerprint`, `card_bin`, `email`, `ip_address`, `country`, `string`, or `case_sensitive_string`. -}
    , valueListListItems :: Object
    {- ^ List of items contained within this value list. -}
    , valueListMetadata :: Object
    {- ^ Set of key-value pairs that you can attach to an object. This can be useful for storing additional information about the object in a structured format. -}
    , valueListName :: Text
    {- ^ The name of the value list. -}
    } deriving (Show, Eq, Generic)


-- Describes: "radar.value_list_item"

data ValueListItem
  = ValueListItem
    { valueListItemId :: Id (RadarValueListItem)
    {- ^ Unique identifier for the object. -}
    , valueListItemObject :: RadarValueListItemObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , valueListItemLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , valueListItemCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , valueListItemCreatedBy :: Text
    {- ^ The name or email address of the user who added this item to the value list. -}
    , valueListItemValue :: Text
    {- ^ The value of the item. -}
    , valueListItemValueList :: Text
    {- ^ The identifier of the value list this item belongs to. -}
    } deriving (Show, Eq, Generic)


