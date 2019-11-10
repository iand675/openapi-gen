{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- Derived from OpenAPI spec
module Stripe.Types.Issuing where

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



-- Describes: "issuing.authorization"

data Authorization
  = Authorization
    { authorizationId :: Id (IssuingAuthorization)
    {- ^ Unique identifier for the object. -}
    , authorizationObject :: IssuingAuthorizationObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , authorizationLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , authorizationApproved :: Bool
    {- ^ Whether the authorization has been approved. -}
    , authorizationAuthorizationMethod :: IssuingAuthorizationAuthorizationMethodEnum
    {- ^ How the card details were provided. One of `chip`, `contactless`, `keyed_in`, `online`, or `swipe`. -}
    , authorizationAuthorizedAmount :: Int
    {- ^ The amount that has been authorized. This will be `0` when the object is created, and increase after it has been approved. -}
    , authorizationAuthorizedCurrency :: Text
    {- ^ The currency that was presented to the cardholder for the authorization. Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must be a [supported currency](https://stripe.com/docs/currencies). -}
    , authorizationBalanceTransactions :: Vector (BalanceTransaction)
    , authorizationCard :: IssuingCard
    , authorizationCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , authorizationHeldAmount :: Int
    {- ^ The amount the authorization is expected to be in `held_currency`. When Stripe holds funds from you, this is the amount reserved for the authorization. This will be `0` when the object is created, and increase after it has been approved. For multi-currency transactions, `held_amount` can be used to determine the expected exchange rate. -}
    , authorizationHeldCurrency :: Text
    {- ^ The currency of the [held amount](https://stripe.com/docs/api#issuing_authorization_object-held_amount). This will always be the card currency. -}
    , authorizationIsHeldAmountControllable :: Bool
    , authorizationMerchantData :: IssuingAuthorizationMerchantData
    , authorizationMetadata :: Object
    {- ^ Set of key-value pairs that you can attach to an object. This can be useful for storing additional information about the object in a structured format. -}
    , authorizationPendingAuthorizedAmount :: Int
    {- ^ The amount the user is requesting to be authorized. This field will only be non-zero during an `issuing.authorization.request` webhook. -}
    , authorizationPendingHeldAmount :: Int
    {- ^ The additional amount Stripe will hold if the authorization is approved. This field will only be non-zero during an `issuing.authorization.request` webhook. -}
    , authorizationRequestHistory :: Vector (IssuingAuthorizationRequest)
    , authorizationStatus :: IssuingAuthorizationStatusEnum
    {- ^ One of `pending`, `reversed`, or `closed`. -}
    , authorizationTransactions :: Vector (IssuingTransaction)
    , authorizationVerificationData :: IssuingAuthorizationVerificationData
    , authorizationWalletProvider :: IssuingAuthorizationWalletProviderEnum
    {- ^ What, if any, digital wallet was used for this authorization. One of `apple_pay`, `google_pay`, or `samsung_pay`. -}
    , authorizationCardholder :: Maybe (Either (Text) (IssuingCardholder))
    {- ^ The cardholder to whom this authorization belongs. -}
    } deriving (Show, Eq, Generic)


-- Describes: "issuing.card"

data Card
  = Card
    { cardId :: Id (IssuingCard)
    {- ^ Unique identifier for the object. -}
    , cardObject :: IssuingCardObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , cardLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , cardAuthorizationControls :: IssuingCardAuthorizationControls
    , cardBrand :: Text
    {- ^ The brand of the card. -}
    , cardCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , cardCurrency :: Text
    {- ^ Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must be a [supported currency](https://stripe.com/docs/currencies). -}
    , cardExpMonth :: Int
    {- ^ The expiration month of the card. -}
    , cardExpYear :: Int
    {- ^ The expiration year of the card. -}
    , cardLast4 :: Text
    {- ^ The last 4 digits of the card number. -}
    , cardMetadata :: Object
    {- ^ Set of key-value pairs that you can attach to an object. This can be useful for storing additional information about the object in a structured format. -}
    , cardName :: Text
    {- ^ The name of the cardholder, printed on the card. -}
    , cardReplacementReason :: IssuingCardReplacementReasonEnum
    {- ^ Why the card that this card replaces (if any) needed to be replaced. One of `damage`, `expiration`, `loss`, or `theft`. -}
    , cardStatus :: IssuingCardStatusEnum
    {- ^ One of `active`, `inactive`, `canceled`, `lost`, `stolen`, or `pending`. -}
    , cardType_ :: IssuingCardTypeEnum
    {- ^ One of `virtual` or `physical`. -}
    , cardCardholder :: Maybe (IssuingCardholder)
    {- ^ The [Cardholder](https://stripe.com/docs/api#issuing_cardholder_object) object to which the card belongs. -}
    , cardPin :: Maybe (IssuingCardPin)
    {- ^ Metadata about the PIN on the card. -}
    , cardReplacementFor :: Maybe (Either (Text) (IssuingCard))
    {- ^ The card this card replaces, if any. -}
    , cardShipping :: Maybe (IssuingCardShipping)
    {- ^ Where and how the card will be shipped. -}
    } deriving (Show, Eq, Generic)


-- Describes: "issuing.card_details"

data CardDetails
  = CardDetails
    { cardDetailsObject :: IssuingCardDetailsObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , cardDetailsCard :: IssuingCard
    , cardDetailsCvc :: Text
    {- ^ The CVC number for the card. -}
    , cardDetailsExpMonth :: Int
    {- ^ The expiration month of the card. -}
    , cardDetailsExpYear :: Int
    {- ^ The expiration year of the card. -}
    , cardDetailsNumber :: Text
    {- ^ The card number. -}
    } deriving (Show, Eq, Generic)


-- Describes: "issuing.card_pin"

data CardPin
  = CardPin
    { cardPinObject :: IssuingCardPinObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , cardPinCard :: IssuingCard
    , cardPinPin :: Maybe (Text)
    {- ^ The PIN (4 digits number) -}
    } deriving (Show, Eq, Generic)


-- Describes: "issuing.cardholder"

data Cardholder
  = Cardholder
    { cardholderId :: Id (IssuingCardholder)
    {- ^ Unique identifier for the object. -}
    , cardholderObject :: IssuingCardholderObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , cardholderLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , cardholderBilling :: IssuingCardholderAddress
    , cardholderCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , cardholderIsDefault :: Bool
    {- ^ Whether or not this cardholder is the default cardholder. -}
    , cardholderMetadata :: Object
    {- ^ Set of key-value pairs that you can attach to an object. This can be useful for storing additional information about the object in a structured format. -}
    , cardholderName :: Text
    {- ^ The cardholder's name. This will be printed on cards issued to them. -}
    , cardholderStatus :: IssuingCardholderStatusEnum
    {- ^ One of `active`, `inactive`, `blocked`, or `pending`. -}
    , cardholderType_ :: IssuingCardholderTypeEnum
    {- ^ One of `individual` or `business_entity`. -}
    , cardholderAuthorizationControls :: Maybe (IssuingCardholderAuthorizationControls)
    , cardholderEmail :: Maybe (Text)
    {- ^ The cardholder's email address. -}
    , cardholderPhoneNumber :: Maybe (Text)
    {- ^ The cardholder's phone number. -}
    } deriving (Show, Eq, Generic)


-- Describes: "issuing.dispute"

data Dispute
  = Dispute
    { disputeId :: Id (IssuingDispute)
    {- ^ Unique identifier for the object. -}
    , disputeObject :: IssuingDisputeObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , disputeLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , disputeAmount :: Int
    {- ^ Disputed amount. Usually the amount of the `disputed_transaction`, but can differ (usually because of currency fluctuation or because only part of the order is disputed). -}
    , disputeCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , disputeCurrency :: Text
    {- ^ The currency the `disputed_transaction` was made in. -}
    , disputeDisputedTransaction :: Either (Text) (IssuingTransaction)
    {- ^ The transaction being disputed. -}
    , disputeEvidence :: IssuingDisputeEvidence
    , disputeMetadata :: Object
    {- ^ Set of key-value pairs that you can attach to an object. This can be useful for storing additional information about the object in a structured format. Individual keys can be unset by posting an empty value to them. All keys can be unset by posting an empty value to `metadata`. -}
    , disputeReason :: IssuingDisputeReasonEnum
    {- ^ Reason for this dispute. One of `other` or `fraudulent`. -}
    , disputeStatus :: IssuingDisputeStatusEnum
    {- ^ Current status of dispute. One of `unsubmitted`, `under_review`, `won`, or `lost`. -}
    } deriving (Show, Eq, Generic)


-- Describes: "issuing.settlement"

data Settlement
  = Settlement
    { settlementId :: Id (IssuingSettlement)
    {- ^ Unique identifier for the object. -}
    , settlementObject :: IssuingSettlementObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , settlementLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , settlementBin :: Text
    {- ^ The Bank Identification Number reflecting this settlement record. -}
    , settlementClearingDate :: Int
    {- ^ The date that the transactions are cleared and posted to user's accounts. -}
    , settlementCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , settlementCurrency :: Text
    {- ^ Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must be a [supported currency](https://stripe.com/docs/currencies). -}
    , settlementInterchangeFees :: Int
    {- ^ The total interchange received as reimbursement for the transactions. -}
    , settlementMetadata :: Object
    {- ^ Set of key-value pairs that you can attach to an object. This can be useful for storing additional information about the object in a structured format. -}
    , settlementNetTotal :: Int
    {- ^ The total net amount required to settle with the network. -}
    , settlementNetworkFees :: Int
    {- ^ The total amount of fees owed to the network. -}
    , settlementNetworkSettlementIdentifier :: Text
    {- ^ The Settlement Identification Number assigned by the network. -}
    , settlementSettlementService :: IssuingSettlementSettlementServiceEnum
    {- ^ One of `international` or `uk_national_net`. -}
    , settlementTransactionCount :: Int
    {- ^ The total number of transactions reflected in this settlement. -}
    , settlementTransactionVolume :: Int
    {- ^ The total transaction amount reflected in this settlement. -}
    } deriving (Show, Eq, Generic)


-- Describes: "issuing.transaction"

data Transaction
  = Transaction
    { transactionId :: Id (IssuingTransaction)
    {- ^ Unique identifier for the object. -}
    , transactionObject :: IssuingTransactionObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , transactionLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , transactionAmount :: Int
    , transactionCard :: Either (Text) (IssuingCard)
    {- ^ The card used to make this transaction. -}
    , transactionCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , transactionCurrency :: Text
    {- ^ Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must be a [supported currency](https://stripe.com/docs/currencies). -}
    , transactionMerchantAmount :: Int
    , transactionMerchantCurrency :: Text
    , transactionMerchantData :: IssuingAuthorizationMerchantData
    , transactionMetadata :: Object
    {- ^ Set of key-value pairs that you can attach to an object. This can be useful for storing additional information about the object in a structured format. -}
    , transactionType_ :: IssuingTransactionTypeEnum
    {- ^ One of `capture`, `refund`, `cash_withdrawal`, `refund_reversal`, `dispute`, or `dispute_loss`. -}
    , transactionAuthorization :: Maybe (Either (Text) (IssuingAuthorization))
    {- ^ The `Authorization` object that led to this transaction. -}
    , transactionBalanceTransaction :: Maybe (Either (Text) (BalanceTransaction))
    , transactionCardholder :: Maybe (Either (Text) (IssuingCardholder))
    {- ^ The cardholder to whom this transaction belongs. -}
    , transactionDispute :: Maybe (Either (Text) (IssuingDispute))
    } deriving (Show, Eq, Generic)


-- Describes: "issuing.verification"

data Verification
  = Verification
    { verificationId :: Id (IssuingVerification)
    {- ^ Unique identifier for the object. -}
    , verificationObject :: IssuingVerificationObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , verificationCard :: Text
    {- ^ The id of the `Card` on which the verification was requested -}
    , verificationCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , verificationExpiresAt :: POSIXTime
    {- ^ Timestamp of the expiry for that verification -}
    , verificationScope :: IssuingVerificationScopeEnum
    {- ^ The scope of the verification (one of `card_pin_retrieve` or `card_pin_update`) -}
    , verificationVerificationMethod :: IssuingVerificationVerificationMethodEnum
    {- ^ The method by which the cardholder will be sent a one-time code (one of `email` or `sms`) -}
    } deriving (Show, Eq, Generic)


