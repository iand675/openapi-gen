{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module OpenAPI.Resolve where
import Control.Algebra
import Control.Applicative
import Control.Carrier.Reader
import Control.Effect.Throw
import Control.Effect.Trace
import Control.Lens
import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), Value)
import Data.Aeson.Pointer
import Data.Aeson.Types (parseEither)
import Data.Set (Set)
import qualified Data.Set as S
import OpenAPI.Types
import URI.ByteString

{-

resolve reference
push reference
resolve all within that.
push those references, etc.

-}

data Resolver m k
  = forall a b. (Show a, FromJSON a) => Resolve (Reference a) (Resolved a -> m b) (b -> m k)
  --  | forall a b. Descend (Reference a) (m b) (b -> m k)
deriving instance Functor m => Functor (Resolver m)
instance Effect Resolver where
  handle state handler (Resolve r f k) = Resolve r (\x -> handler (f x <$ state)) (handler . fmap k)
  -- handle state handler (Descend r m k) = Descend r (handler (m <$ state)) (handler . fmap k)

resolve :: (Show a, FromJSON a, Has Resolver sig m) => Reference a -> (Resolved a -> m b) -> m b
resolve ref f = send $ Resolve ref f pure

resolveUnresolved :: (Show a, FromJSON a, Has Resolver sig m) => Unresolved a -> m (Resolved a)
resolveUnresolved (Ref r) = resolve r pure
resolveUnresolved (Obj o) = pure $ Resolved $ Resolution Nothing o

unresolve :: Resolved a -> Unresolved a
unresolve (Resolved (Resolution mref a)) = case mref of
  Nothing -> Obj a
  Just ref -> Ref ref
unresolve (RecursiveReference ref) = Ref ref

resolveAll :: (Show a, FromJSON a, Traversable f, Has Resolver sig m) => f (Unresolved a) -> m (f (Resolved a))
resolveAll = traverse resolveUnresolved

-- TODO This can be nonterminating if references are recursive
resolveAllIn ::
     ( Show (t Unresolved)
     , FromJSON (t Unresolved)
     , Traversable f
     , Has Resolver sig m
     , ResolveIn t
     )
  => f (Unresolved (t Unresolved))
  -> m (f (Resolved (t Resolved)))
resolveAllIn = traverse $ \elem -> case elem of
  Ref r -> resolve r $ \res -> case res of
    (Resolved (Resolution mRef v)) -> Resolved . Resolution (reRef <$> mRef) <$> resolveIn v
    (RecursiveReference ref) -> pure $ RecursiveReference $ reRef ref
  Obj o -> (Resolved . Resolution Nothing) <$> resolveIn o

newtype LocalResolverC m a = LocalResolverC
  { runLocalResolverC :: ReaderC (Set (Reference ())) (ReaderC Value m) a
  }
  deriving newtype
      ( Monad
      , Functor
      , Applicative
      , MonadIO
      , Alternative
      , MonadFail
      )

runLocalResolver :: Value -> LocalResolverC m a -> m a
runLocalResolver x = runReader x . runReader mempty . runLocalResolverC

data JsonError
  = JsonParseError String
  | JsonPointerError String
  deriving (Show)

data RemoteReferencesUnsupported = RemoteReferencesUnsupported
  deriving (Show)

instance ( Has (Throw JsonError) sig m
         , Has (Throw RemoteReferencesUnsupported) sig m
         , Has Trace sig m
         , Algebra sig m
         ) =>
         Algebra (Resolver :+: sig) (LocalResolverC m) where
  alg (L (Resolve p f k)) = do
    trace ("Resolve " ++ show p)
    fullDocument <- LocalResolverC ask
    let anyTypeRef = reRef p :: Reference ()
    alreadySeenRefs <- LocalResolverC ask
    if (S.member anyTypeRef alreadySeenRefs)
      then trace "Encountered recursive reference" >> f (RecursiveReference p) >>= k
      else case p of
        AbsoluteReference _ -> do
          trace "Can't handle remote reference"
          throwError RemoteReferencesUnsupported
        RelativeReference r -> case parseURIFragment r of
          Left err -> do
            trace "Pointer error"
            throwError $ JsonPointerError err
          Right ptr -> case valueAt ptr fullDocument of
            Nothing -> do
              trace "No value error"
              throwError $ JsonPointerError ("Unable to locate value at reference: " <> show ptr)
            Just x -> case parseEither parseJSON x of
              Left err -> do
                trace "Parse failure"
                throwError $ JsonParseError err
              Right ok -> do
                trace "OK!"
                LocalResolverC (local (S.insert (reRef p :: Reference ())) (runLocalResolverC (f (Resolved $ Resolution (Just p) ok)))) >>= k
  -- alg (L (Descend p m k)) =  >>= k
  alg (R other) = LocalResolverC (handleCoercible other)

class ResolveIn t where
  resolveIn :: (Has Resolver sig m) => t Unresolved -> m (t Resolved)
  unresolveIn :: t Resolved -> t Unresolved

instance ResolveIn Root where
  resolveIn r = do
    rootComponents' <- resolveIn (r ^. components)
    rootPaths' <- (traverse . traverse) resolveIn (r ^. paths)
    pure $ r
      { rootComponents = rootComponents'
      , rootPaths = rootPaths'
      }
  unresolveIn r = r
    { rootComponents = unresolveIn $ rootComponents r
    , rootPaths = fmap unresolveIn <$> rootPaths r
    }

instance ResolveIn Components where
  resolveIn c = do
    componentsSchemas <- resolveAllIn (c ^. schemas)
    componentsResponses <- resolveAllIn (c ^. responses)
    componentsParameters <- resolveAllIn (c ^. parameters)
    componentsExamples <- resolveAll (c ^. examples)
    componentsRequestBodies <- resolveAllIn (c ^. requestBodies)
    componentsHeaders <- resolveAllIn (c ^. headers)
    componentsSecuritySchemes <- resolveAll (c ^. securitySchemes)
    componentsLinks <- resolveAll (c ^. links)
    componentsCallbacks <- resolveAll (c ^. callbacks)
    pure Components{..}
  unresolveIn c = c
    { componentsSchemas = unresolve . fmap unresolveIn <$> componentsSchemas c
    , componentsResponses = unresolve . fmap unresolveIn <$> componentsResponses c
    , componentsParameters = unresolve . fmap unresolveIn <$> componentsParameters c
    , componentsExamples = unresolve <$> componentsExamples c
    , componentsRequestBodies = unresolve . fmap unresolveIn <$> componentsRequestBodies c
    , componentsHeaders = unresolve . fmap unresolveIn <$> componentsHeaders c
    , componentsSecuritySchemes = unresolve <$> componentsSecuritySchemes c
    , componentsLinks = unresolve <$> componentsLinks c
    , componentsCallbacks = unresolve <$> componentsCallbacks c
    }

instance ResolveIn Parameter where
  resolveIn p = do
    parameterSchema' <- resolveAllIn (p ^. schema)
    parameterExamples' <- resolveAll (p ^. examples)
    parameterContent' <- traverse resolveIn (p ^. content)
    pure p { parameterSchema = parameterSchema'
           , parameterExamples = parameterExamples'
           , parameterContent = parameterContent'
           }
  unresolveIn p = p
    { parameterSchema = unresolve . fmap unresolveIn <$> parameterSchema p
    , parameterExamples = unresolve <$> parameterExamples p
    , parameterContent = unresolveIn <$> parameterContent p
    }

instance ResolveIn RequestBody where
  resolveIn r = do
    c <- traverse resolveIn (r ^. content)
    pure $ r { requestBodyContent = c }
  unresolveIn r = r { requestBodyContent = unresolveIn <$> requestBodyContent r }

instance ResolveIn MediaType where
  resolveIn m = do
    mediaTypeSchema' <- resolveAllIn (m ^. schema)
    mediaTypeExamples' <- resolveAll (m ^. examples)
    mediaTypeEncoding' <- traverse resolveIn (m ^. encoding)
    pure $ m { mediaTypeSchema = mediaTypeSchema'
             , mediaTypeExamples = mediaTypeExamples'
             , mediaTypeEncoding = mediaTypeEncoding'
             }
  unresolveIn m = m
    { mediaTypeSchema = (unresolve . fmap unresolveIn) <$> mediaTypeSchema m
    , mediaTypeExamples = unresolve <$> mediaTypeExamples m
    , mediaTypeEncoding = unresolveIn <$> mediaTypeEncoding m
    }

instance ResolveIn Encoding where
  resolveIn e = do
    h <- resolveAllIn (e ^. headers)
    pure $ e {encodingHeaders = h}
  unresolveIn e =
    e {encodingHeaders = unresolve . fmap unresolveIn <$> encodingHeaders e}

instance ResolveIn Responses where
  resolveIn (Responses r) = Responses <$> traverse resolveIn r
  unresolveIn (Responses r) = Responses (fmap unresolveIn r)

instance ResolveIn Response where
  resolveIn r = do
    responseContent' <- resolveAllIn (r ^. content)
    responseHeaders' <- resolveAllIn (r ^. headers)
    responseLinks' <- resolveAll (r ^. links)
    pure $ r
      { responseContent = responseContent'
      , responseHeaders = responseHeaders'
      , responseLinks = responseLinks'
      }
  unresolveIn r = r
    { responseContent = unresolve . fmap unresolveIn <$> responseContent r
    , responseHeaders = unresolve . fmap unresolveIn <$> responseHeaders r
    , responseLinks = unresolve <$> responseLinks r
    }

instance ResolveIn Schema where
  resolveIn s = do
    schemaProperties' <- resolveAllIn (s ^. properties)
    schemaAnyOf' <- resolveAllIn (s ^. OpenAPI.Types.anyOf)
    schemaItems' <- resolveAllIn (s ^. items)
    schemaAdditionalProperties' <- traverse resolveIn (s ^. additionalProperties)
    pure $ s
      { schemaProperties = schemaProperties'
      , schemaAnyOf = schemaAnyOf'
      , schemaItems = schemaItems'
      , schemaAdditionalProperties = schemaAdditionalProperties'
      }
  unresolveIn s = s
    { schemaProperties = unresolve . fmap unresolveIn <$> schemaProperties s
    , schemaAnyOf = unresolve . fmap unresolveIn <$> schemaAnyOf s
    , schemaItems = unresolve . fmap unresolveIn <$> schemaItems s
    , schemaAdditionalProperties = unresolveIn <$> schemaAdditionalProperties s
    }


instance ResolveIn AdditionalProperties where
  resolveIn (AdditionalPropertiesToggle b) =  pure $ AdditionalPropertiesToggle b
  resolveIn (AdditionalPropertiesSchema u) = do
    r <- resolveUnresolved u
    case r of
      (Resolved (Resolution ref s)) -> do
        s' <- resolveIn s
        let ref' = resolvedReference <$> ref
        pure $ AdditionalPropertiesSchema $ Resolved (Resolution ref' s')
      (RecursiveReference ref) -> pure $ AdditionalPropertiesSchema $ RecursiveReference $ reRef ref
  unresolveIn (AdditionalPropertiesToggle b) = AdditionalPropertiesToggle b
  unresolveIn (AdditionalPropertiesSchema u) = AdditionalPropertiesSchema (unresolveIn <$> unresolve u)

instance ResolveIn ApiEndpoint where
  resolveIn a = do
    ps <- traverse resolveIn (a ^. parameters)
    req <- resolveIn (a ^. requestBody)
    resps <- resolveIn (a ^. responses)
    pure $ a
      { apiEndpointParameters = ps
      , apiEndpointRequestBody = req
      , apiEndpointResponses = resps
      }
  unresolveIn a = a
    { apiEndpointParameters = unresolveIn <$> apiEndpointParameters a
    , apiEndpointRequestBody = unresolveIn $ apiEndpointRequestBody a
    , apiEndpointResponses = unresolveIn $ apiEndpointResponses a
    }

{-
  do
    r <- resolveIn o
    pure $ Resolved Nothing r
-}
-- Resolver options:
-- local only
-- remote
-- preloaded remote
