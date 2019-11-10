{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module OpenAPI.Support
  (
  -- Aeson support, primitive type support
    Text
  , Vector
  , Scientific
  , POSIXTime
  -- Aeson stuff for real
  , ToJSON(..)
  , FromJSON(..)
  , Object
  , withText
  , withObject
  , object
  , pairs
  , (.=)
  , (.:)
  , (.:?)
  --
  , KnownResponse
  , RequestBuilder(..)
  , HttpResponseError(..)
  , FromHttpResponse(..)
  , RequestMethod
  , RequestPath
  -- Utility Functions
  , (>?>)
  -- Heavy lifting for request wiring
  , HttpRequest(..)
  , HttpResponse(..)
  , Http(..)
  , http
  , sendRequest
  , HttpC
  , runHttp
  -- Type literal standard http methods
  , OPTIONS
  , GET
  , HEAD
  , PUT
  , POST
  , DELETE
  , PATCH
  -- Utility functions
  , idempotentHttp
  , Idempotency(..)
  , MethodIdempotency
  , Safety(..)
  , MethodSafety
  , IdempotencyKey(..)
  , AnyOf
  ) where

import Data.Aeson as X
import Control.Applicative (Alternative(..))
import Control.Algebra
import Control.Effect.Error
import Control.Carrier.Interpret
import Control.Carrier.Reader
import Control.Effect.Sum
import Control.Effect.State
import Control.Carrier.State.Strict
import Control.Exception (Exception(..), SomeException)
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary.Builder (toLazyByteString)
import Data.Functor.Const
import Data.Kind
import Data.Scientific (Scientific)
import Data.Sum
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable
import Data.Vector (Vector)
import Data.Void
import Data.Proxy
import GHC.Generics
       (C1, D1, Generic, Generic1, Meta(MetaCons), Rep1, from1,
        unM1)
import qualified GHC.Generics as G
import GHC.TypeLits
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types
import Network.HTTP.Types.QueryLike as X

instance Apply ToJSON1 fs => ToJSON1 (Sum fs) where
  liftToJSON fOne fMany = apply @ToJSON1 (liftToJSON fOne fMany)
  liftToEncoding fOne fMany = apply @ToJSON1 (liftToEncoding fOne fMany)

instance FromJSON1 (Sum '[]) where
  liftParseJSON _ _ v = fail ("Unable to match value " <> show v <> " with any Sum members")

instance (FromJSON1 f, FromJSON1 (Sum fs), Element f (f ': fs)) => FromJSON1 (Sum (f ': fs)) where
  liftParseJSON :: forall a. (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser (Sum (f ': fs) a)
  liftParseJSON fOne fMany v = (inject <$> parseJsonF) <|> (weaken <$> parseJsonFs)
    where
      parseJsonF :: Parser (f a)
      parseJsonF = liftParseJSON fOne fMany v
      parseJsonFs :: Parser (Sum fs a)
      parseJsonFs = liftParseJSON fOne fMany v

type AnyOf a = Sum a Void

class RequestBuilder req where
  type Response req :: *
  -- TODO endpointServer ::
  endpointMethod :: req -> Method
  endpointPath :: req -> [Text]
  endpointQuery :: req -> Query
  endpointHeaders :: req -> [Header]
  -- TODO endpointCookies :: req -> ByteString

data HttpResponseError resp
  = HttpError SomeException
  | DecodingError (DecodingError resp)

deriving instance (Show (DecodingError resp)) => Show (HttpResponseError resp)

instance (Typeable resp, Show (DecodingError resp)) => Exception (HttpResponseError resp)

type family KnownResponse req :: *

class FromHttpResponse resp where
  type DecodingError resp :: *
  type DecodingError resp = String
  decodeResponse ::
       (Has (Error (HttpResponseError resp)) sig m)
    => HttpResponse
    -> m resp

type family RequestMethod a :: Symbol
type family RequestPath a :: Symbol

(>?>) mx l = case mx of
  Nothing -> l
  Just x -> x : l
infixr 5 >?>
{-# INLINE (>?>) #-}

newtype HttpRequest = HttpRequest HTTP.Request
-- TODO more than lbs support
newtype HttpResponse = HttpResponse (HTTP.Response L.ByteString)

data Http (m :: * -> *) k
  = SendRequest HttpRequest (HttpResponse -> m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (Effect)

sendRequest :: (Has Http sig m) => HttpRequest -> m HttpResponse
sendRequest r = send $ SendRequest r pure

instance HasConstructorName (Http m)
-- data Server

http ::
     ( Has (Reader HttpRequest) sig m
     , Has (Error (HttpResponseError (Response req))) sig m
     , Has Http sig m
     , RequestBuilder req
     , FromHttpResponse (Response req)
     )
  => req
  -> m (Response req)
http r = do
  (HttpRequest req) <- ask
  let builtReq = req
        { HTTP.method = endpointMethod r
        , HTTP.path = L.toStrict $ toLazyByteString $ encodePathSegments $ endpointPath r
        , HTTP.queryString = if B.null (HTTP.queryString req)
            then renderQuery True $ endpointQuery r
            else HTTP.queryString req <> "&" <> renderQuery False (endpointQuery r)
        , HTTP.requestHeaders = {- ("Cookies", _) : -} (HTTP.requestHeaders req <> endpointHeaders r)
        }
  r <- send $ SendRequest (HttpRequest builtReq) pure
  decodeResponse r

data StripeRequest (m :: * -> *) k
  = StripeRequest HttpRequest (HttpResponse -> m k)
  deriving stock (Functor, Generic1)
  deriving anyclass (Effect)

newtype StripeApi m a = StripeApi { runStripe :: m a }
 deriving newtype
      ( Monad
      , Functor
      , Applicative
      , MonadIO
      , Alternative
      )

instance (Algebra sig m, Has Http sig m) => Algebra (StripeRequest :+: sig) (StripeApi m) where
  alg = \case
    L (StripeRequest req k) -> StripeApi (sendRequest req) >>= k
    R other -> StripeApi $ handleCoercible other

class HasConstructorName f where
  constructorName :: f a -> String
  default constructorName :: (Generic1 f, HasConstructorName (Rep1 f)) => f a -> String
  constructorName = constructorName . from1

instance HasConstructorName c1 => HasConstructorName (D1 meta c1) where
  constructorName = constructorName . unM1

instance (HasConstructorName l, HasConstructorName r) => HasConstructorName (l G.:+: r) where
  constructorName (G.L1 l) = constructorName l
  constructorName (G.R1 r) = constructorName r

instance KnownSymbol str => HasConstructorName (C1 ('MetaCons str fixity hasSelectors) s1) where
  constructorName _ = symbolVal (Proxy @str)

newtype LogEffectCalls (eff :: (* -> *) -> * -> *) m a = LogEffectCalls { logEffectCalls :: m a }
 deriving newtype
      ( Monad
      , Functor
      , Applicative
      , MonadIO
      , Alternative
      )


instance ( MonadIO m
         , Algebra sig m
         , Effect eff
         , Member eff sig
         , HasConstructorName (eff (LogEffectCalls eff m))
         ) =>
         Algebra (eff :+: sig) (LogEffectCalls eff m) where
  alg (L op) = LogEffectCalls $ do
    liftIO $ putStrLn "start!"
    liftIO $ putStrLn $ constructorName op
    r <- handleCoercible op
    liftIO $ putStrLn "finished!"
    pure r
  alg (R other) = LogEffectCalls (handleCoercible other)

type OPTIONS = "OPTIONS"
type GET = "GET"
type HEAD = "HEAD"
type PUT = "PUT"
type POST = "POST"
type DELETE = "DELETE"
type PATCH = "PATCH"

data Idempotency = Idempotent | NonIdempotent
type family MethodIdempotency (k :: Symbol) :: Idempotency

type instance MethodIdempotency OPTIONS = 'Idempotent
type instance MethodIdempotency GET = 'Idempotent
type instance MethodIdempotency HEAD = 'Idempotent
type instance MethodIdempotency PUT = 'Idempotent
type instance MethodIdempotency POST = 'NonIdempotent
type instance MethodIdempotency DELETE = 'Idempotent
type instance MethodIdempotency PATCH = 'NonIdempotent

data Safety = Safe | Unsafe
type family MethodSafety (k :: Symbol) :: Safety

type instance MethodSafety OPTIONS = 'Safe
type instance MethodSafety GET = 'Safe
type instance MethodSafety HEAD = 'Safe
type instance MethodSafety PUT = 'Unsafe
type instance MethodSafety POST = 'Unsafe
type instance MethodSafety DELETE = 'Unsafe
type instance MethodSafety PATCH = 'Unsafe

data IdempotencyKey = IdempotencyKey ByteString

newtype HttpC m a
  = HttpC { runHttp :: (m a) }
  deriving newtype (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus)

instance MonadTrans HttpC where
  lift = HttpC

instance
     -- So long as the 'm' monad can interpret the 'sig' effects (and also
     -- perform IO)...
     ( MonadIO m
     , Algebra sig m
     )
     -- ... the 'HttpC m' monad can interpret 'Http :+: sig' effects
  => Algebra (Http :+: sig) (HttpC m) where

  -- eff :: forall a. (Http :+: sig) (HttpC m) a -> HttpC m a
  alg = \case
    L (SendRequest (HttpRequest req) k) ->
      HttpC $ do
        resp <- liftIO (HTTP.getGlobalManager >>= HTTP.httpLbs req)
        runHttp $ k $ HttpResponse resp

    R other -> HttpC (handleCoercible other)

-- | See Stripe's docs on idempotency keys
idempotentHttp ::
     ( Has (Reader HttpRequest) sig m
     , Has (Error (HttpResponseError (Response req))) sig m
     , Has Http sig m
     , RequestBuilder req
     , FromHttpResponse (Response req)
     , MethodIdempotency (RequestMethod req) ~ 'NonIdempotent
     )
  => IdempotencyKey
  -> req
  -> m (Response req)
idempotentHttp (IdempotencyKey k) r = local
  (\(HttpRequest r) -> HttpRequest $ r { HTTP.requestHeaders = ("Idempotency-Key", k) : HTTP.requestHeaders r})
  (http r)

-- deriving instance (Functor m) => Generic1 (State s m)
instance HasConstructorName (State s m)

example4 :: IO (Int, ())
example4 = runReader ("hello" :: String) . runState 0 $ logEffectCalls @(State Int) $ do
  list <- ask
  liftIO (putStrLn list)
  put (length list)
