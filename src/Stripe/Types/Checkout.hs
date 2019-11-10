{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- Derived from OpenAPI spec
module Stripe.Types.Checkout where

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



-- Describes: "checkout.session"

data Session
  = Session
    { sessionId :: Id (CheckoutSession)
    {- ^ Unique identifier for the object. Used to pass to `redirectToCheckout`
in Stripe.js. -}
    , sessionObject :: CheckoutSessionObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , sessionLivemode :: Bool
    {- ^ Has the value `true` if the object exists in live mode or the value `false` if the object exists in test mode. -}
    , sessionCancelUrl :: Text
    {- ^ The URL the customer will be directed to if they decide to cancel payment and return to your website. -}
    , sessionDisplayItems :: Vector (CheckoutSessionDisplayItem)
    {- ^ The line items, plans, or SKUs purchased by the customer. -}
    , sessionLocale :: CheckoutSessionLocaleEnum
    {- ^ The IETF language tag of the locale Checkout is displayed in. If blank
or `auto`, the browser's locale is used. -}
    , sessionMode :: CheckoutSessionModeEnum
    {- ^ The mode of the Checkout Session, one of `payment`, `setup`, or `subscription`. -}
    , sessionPaymentMethodTypes :: Vector (Text)
    {- ^ A list of the types of payment methods (e.g. card) this Checkout
Session is allowed to accept. -}
    , sessionSubmitType :: CheckoutSessionSubmitTypeEnum
    {- ^ Describes the type of transaction being performed by Checkout in order
to customize relevant text on the page, such as the submit button.
`submit_type` can only be specified on Checkout Sessions using line
items or a SKU, but not Checkout Sessions for subscriptions. Supported
values are
`auto`, `book`, `donate`, or `pay`. -}
    , sessionSuccessUrl :: Text
    {- ^ The URL the customer will be directed to after the payment or
subscription creation is successful. -}
    , sessionBillingAddressCollection :: Maybe (Text)
    {- ^ The value (`auto` or `required`) for whether Checkout collected the
customer's billing address. -}
    , sessionClientReferenceId :: Maybe (Text)
    {- ^ A unique string to reference the Checkout Session. This can be a
customer ID, a cart ID, or similar, and can be used to reconcile the
session with your internal systems. -}
    , sessionCustomer :: Maybe (Either (Text) (Customer))
    {- ^ The ID of the customer for this session. A new customer will be created
unless an existing customer was provided in when the session was
created. -}
    , sessionCustomerEmail :: Maybe (Text)
    {- ^ If provided, this value will be used when the Customer object is created.
If not provided, customers will be asked to enter their email address.
Use this parameter to prefill customer data if you already have an email
on file. To access information about the customer once a session is
complete, use the `customer` field. -}
    , sessionPaymentIntent :: Maybe (Either (Text) (PaymentIntent))
    {- ^ The ID of the PaymentIntent for `payment` mode. -}
    , sessionSetupIntent :: Maybe (Either (Text) (SetupIntent))
    {- ^ The ID of the SetupIntent if mode was set to `setup`. -}
    , sessionSubscription :: Maybe (Either (Text) (Subscription))
    {- ^ The ID of the subscription created if one or more plans were provided. -}
    } deriving (Show, Eq, Generic)


