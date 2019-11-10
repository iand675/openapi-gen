{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenAPI.Gen where
import           Control.Algebra
import           Control.Effect.Trace
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Lens hiding ((.=))
import           Control.Lens.TH
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Coerce
import           Data.Functor.Classes
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, fromJust)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Manipulate as T
import           Data.Tree (Tree(..))
import qualified Data.Vector as V
import           GHC.Generics (Generic1, Generic)
import qualified Network.HTTP.Media as Media
import           URI.ByteString
import           OpenAPI.Gen.Coders
import           OpenAPI.Gen.Identifier
import           OpenAPI.Resolve
import           OpenAPI.Types


-- | Return the first monadic action result that succeeds
firstSuccess :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstSuccess [] = pure Nothing
firstSuccess (mm : next) = do
  m <- mm
  case m of
    Nothing -> firstSuccess next
    Just _ -> pure m

-- TODO this might be useful in OpenAPI.Types, not sure. Would also be good to track whether names have been munged
data SchemaParent f
  = SchemaParentComponents Text (Components f)
  | SchemaParentParameter Text (Parameter f)
  | SchemaParentSchemaProperty Text (Schema f)
  | SchemaParentSchemaAnyOf Int (Schema f)
  | SchemaParentSchemaItems (Schema f)
  | SchemaParentMediaType (MediaType f)

deriving instance (Show1 f) => Show (SchemaParent f)
deriving instance (Eq1 f) => Eq (SchemaParent f)

segmentName :: SchemaParent f -> Text
segmentName (SchemaParentComponents t _) = t
segmentName (SchemaParentParameter t _) = t
segmentName (SchemaParentSchemaProperty t _) = t
segmentName (SchemaParentSchemaAnyOf n _) = T.pack $ show n
segmentName (SchemaParentSchemaItems _) = "items"
segmentName (SchemaParentMediaType _) = "mediaType"

-- $ Data types used in output

data AnnotatedIdent a = AnnotatedIdent
  { annotatedIdentAnnotation :: a
  , annotatedIdent :: ScopedIdent
  } deriving (Eq)

annotate :: ScopedIdent -> a -> AnnotatedIdent a
annotate = flip AnnotatedIdent

data ChildFormat = Parens | TypeList
  deriving (Eq)

data IdentContext = IdentContext
  { identContextInstanceVia :: Maybe Text
  -- ^ Allow providing instances ala a newtype without exposing the newtype as part of the public datatype
  , identChildFormat :: ChildFormat
  } deriving (Eq)

standardIdent :: IdentContext
standardIdent = IdentContext Nothing Parens

stdAnn :: ScopedIdent -> AnnotatedIdent IdentContext
stdAnn s = annotate s standardIdent

data RenderedType = RenderedType
  { renderedTypeName :: Tree (AnnotatedIdent IdentContext)
  } deriving (Eq)

instance Show RenderedType where
  show (RenderedType tyTree) = go tyTree
    where
      wrapper :: ChildFormat -> [Tree (AnnotatedIdent IdentContext)] -> String
      wrapper Parens t = intercalate " " $ map (\f -> "(" ++ go f ++ ")") t
      wrapper TypeList t = " '[" ++ intercalate ", " (map go t) ++ "]"

      go :: Tree (AnnotatedIdent IdentContext) -> String
      go (Node (AnnotatedIdent IdentContext{..} ident) children) = T.unpack (qualifyScoped ident) ++ case children of
        [] -> ""
        _ -> " " ++ wrapper identChildFormat children

instance ToJSON RenderedType where
  toJSON = toJSON . show

data TypedParameter = TypedParameter
  { typedParameterType :: RenderedType
  , typedParameterParam :: Parameter Unresolved
  } deriving (Show, Eq)

instance ToJSON TypedParameter where
  toJSON TypedParameter{..} = object
    [ "type" .= typedParameterType
    , "param" .= typedParameterParam
    ]

data ResolvedPathSegment = ResolvedPathSegment
  { resolvedPathSegment :: PathSegment
  , resolvedPathSegmentParameter :: Maybe (Parameter Unresolved)
  } deriving (Show, Eq)

instance ToJSON ResolvedPathSegment where
  toJSON ResolvedPathSegment{..} = case resolvedPathSegment of
    ConstSegment bs -> object
      [ "type" .= ("const" :: Text)
      , "raw" .= rawSegment resolvedPathSegment
      ]
    NamedSegment bs -> object
      [ "type" .= ("named" :: Text)
      , "param" .= resolvedPathSegmentParameter
      ]

data Response = Response
  { responseContentType :: MT
  , responsePattern :: ResponseKey
  , responsePatternPredicate :: Text
  , responseType :: RenderedType
  , responseSuffix :: Text -- Used to generate enum with all different response kinds. This should be as pretty as possible
  , responseDecoderFunction :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON OpenAPI.Gen.Response where
  toJSON OpenAPI.Gen.Response{..} = object
    [ "suffix" .= responseSuffix
    , "contentType" .= responseContentType
    , "pattern" .= responsePattern
    , "predicate" .= responsePatternPredicate
    , "type" .= responseType
    , "decoderFunction" .= responseDecoderFunction
    ]

data NestedResponse = NestedResponse
  { nestedResponseTypeName :: Text
  , nestedResponseContentTypes :: M.Map MT OpenAPI.Gen.Response
  , nestedResponseInstances :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON NestedResponse where
  toJSON NestedResponse{..} = object
    [ "typeName" .= nestedResponseTypeName
    , "contentTypes" .= nestedResponseContentTypes
    , "instances" .= nestedResponseInstances
    ]

prettyResponseSuffix ::
     Maybe MT -- ^ Content type. @Nothing@ if we don't want to add the content type to the suffix.
  -> ResponseKey
  -> Text
prettyResponseSuffix mct pat = prettyMime mct <> prettyPat pat
  where
    prettyMime Nothing = ""
    prettyMime (Just ct) = ""
    prettyPat DefaultResponse = "Default"
    prettyPat (ConstStatus statusCode) = fromMaybe (T.pack $ show pat) $ lookup statusCode
      [ (100, "Continue")
      , (101, "SwitchingProtocols")
      , (102, "Processing")
      , (200, "OK")
      , (201, "Created")
      , (202, "Accepted")
      , (203, "NonAuthoritativeInformation")
      , (204, "NoContent")
      , (205, "ResetContent")
      , (206, "PartialContent")
      , (207, "MultiStatus")
      , (208, "AlreadyReported")
      , (226, "IMUsed")
      , (300, "MultipleChoices")
      , (301, "MovedPermanently")
      , (302, "Found")
      , (303, "SeeOther")
      , (304, "NotModified")
      , (305, "UseProxy")
      , (307, "TemporaryRedirect")
      , (308, "PermanentRedirect")
      , (400, "BadRequest")
      , (401, "Unauthorized")
      , (402, "PaymentRequired")
      , (403, "Forbidden")
      , (404, "NotFound")
      , (405, "MethodNotAllowed")
      , (406, "NotAcceptable")
      , (407, "ProxyAuthenticationRequired")
      , (408, "RequestTimeout")
      , (409, "Conflict")
      , (410, "Gone")
      , (411, "LengthRequired")
      , (412, "PreconditionFailed")
      , (413, "PayloadTooLarge")
      , (414, "RequestURITooLong")
      , (415, "UnsupportedMediaType")
      , (416, "RequestedRangeNotSatisfiable")
      , (417, "ExpectationFailed")
      , (418, "I'mATeapot")
      , (421, "MisdirectedRequest")
      , (422, "UnprocessableEntity")
      , (423, "Locked")
      , (424, "FailedDependency")
      , (426, "UpgradeRequired")
      , (428, "PreconditionRequired")
      , (429, "TooManyRequests")
      , (431, "RequestHeaderFieldsTooLarge")
      , (444, "ConnectionClosedWithoutResponse")
      , (451, "UnavailableForLegalReasons")
      , (499, "ClientClosedRequest")
      , (500, "InternalServerError")
      , (501, "NotImplemented")
      , (502, "BadGateway")
      , (503, "ServiceUnavailable")
      , (504, "GatewayTimeout")
      , (505, "HTTPVersionNotSupported")
      , (506, "VariantAlsoNegotiates")
      , (507, "InsufficientStorage")
      , (508, "LoopDetected")
      , (510, "NotExtended")
      , (511, "NetworkAuthenticationRequired")
      , (599, "NetworkConnectTimeoutError")
      ]
    prettyPat (StatusPatternResponse (Digit n) Wildcard Wildcard) = case n of
      1 -> "Informational"
      2 -> "Success"
      3 -> "Redirection"
      4 -> "ClientError"
      5 -> "ServerError"
      _ -> T.pack $ show pat
    prettyPat _ = T.pack $ show pat


data Endpoint = Endpoint
  { endpointMethod :: Text
  , endpointDescription :: CommonMark
  , endpointName :: Text
  , endpointPath :: Path
  , endpointDeprecated :: Bool
  , endpointParameters :: V.Vector TypedParameter
  , endpointRequestBodyRequired :: Bool
  , endpointRequestBodies :: H.HashMap Text (MediaType Unresolved)
  -- Resolves path template segments with given parameters
  , endpointResolvedPath :: [ResolvedPathSegment]
  , endpointResolvedQuery :: V.Vector (Parameter Unresolved)
  , endpointResolvedHeaders :: V.Vector (Parameter Unresolved)
  , endpointResolvedCookie :: V.Vector (Parameter Unresolved)
  , endpointFlatResponses :: [OpenAPI.Gen.Response]
  , endpointNestedResponses :: H.HashMap ResponseKey NestedResponse
  -- TODO other useful stuff
  } deriving (Show, Eq, Generic)

instance ToJSON Endpoint

data GlobalGeneratorState = GlobalGeneratorState
  { globalGeneratorStateDeclaredTypes :: IdentTree DataType
  , globalGeneratorStateDeclaredEnums :: IdentTree FieldEnum
  , globalGeneratorStateDeclaredEndpoints :: [Endpoint]
  }

data DataType = DataType
  { typeName :: Text
  , constructors :: [Constructor]
  , typeOriginalName :: Text
  , typeDescription :: Maybe Text
  , codings :: Coders
  , instances :: [Text] -- Rendered instances
  } deriving (Show, Eq, Generic)

instance ToJSON DataType

data Constructor = Constructor
  { constructorName :: Text
  , constructorFields :: [ConstructorField]
  } deriving (Show, Eq, Generic)

instance ToJSON Constructor

data ConstructorField = ConstructorField
  { fieldName :: Text
  , fieldOriginalName :: Text
  , fieldType :: Text
  , fieldNullable :: Bool
  , fieldDescription :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ConstructorField

data EnumOption = EnumOption
  { enumOptionName :: Text
  , enumOptionOriginal :: Text
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON EnumOption where
  toJSON EnumOption{..}= object
    [ "name" .= enumOptionName
    , "original" .= enumOptionOriginal
    ]

data FieldEnum = FieldEnum
  { fieldEnumName :: ScopedIdent
  , fieldEnumOriginal :: Text
  , fieldEnumOptions :: V.Vector EnumOption
  -- , fieldEnumScope :: NonEmpty Text
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON FieldEnum where
  toJSON FieldEnum{..} = object
    [ "name" .= fieldEnumName
    , "original" .= fieldEnumOriginal
    , "options" .= fieldEnumOptions
    ]

makeFields ''GlobalGeneratorState

emptyGlobalGeneratorState :: GlobalGeneratorState
emptyGlobalGeneratorState = GlobalGeneratorState emptyIdentTree emptyIdentTree mempty

-- $ Traversals over schemas in various parts of an OpenAPI spec

componentTopLevelSchemas :: Traversable r => Traversal' (Components r) (Schema r)
componentTopLevelSchemas fa Components{..} = (\s resp param req hdr -> Components s resp param componentsExamples req hdr componentsSecuritySchemes componentsLinks componentsCallbacks)
  <$> (each . traverse) fa componentsSchemas
  <*> (each . traverse . content . each . traverse . schema . _Just . traverse) fa componentsResponses
  <*> (each . traverse . schema . _Just . traverse) fa componentsParameters
  <*> (each . traverse . content . each . schema . _Just . traverse) fa componentsRequestBodies
  <*> (each . traverse . schema . _Just . traverse) fa componentsHeaders

pathsTopLevelSchemas :: Traversable r => Traversal' (H.HashMap Path (H.HashMap Method (ApiEndpoint r))) (Schema r)
pathsTopLevelSchemas = each . each . apiEndpointTopLevelSchemas

apiEndpointTopLevelSchemas :: Traversable r => Traversal' (ApiEndpoint r) (Schema r)
apiEndpointTopLevelSchemas fa ApiEndpoint{..} = ApiEndpoint apiEndpointDescription apiEndpointOperationId <$> traverseParams fa apiEndpointParameters <*> traverseRequestBody fa apiEndpointRequestBody <*> traverseResponses fa apiEndpointResponses <*> pure apiEndpointServers <*> pure apiEndpointDeprecated
  where
    traverseParams = each . schema . _Just . traverse
    traverseRequestBody = content . each . schema . _Just . traverse
    traverseResponses = responses . each . content . each . traverse . schema . _Just . traverse

-- This is maybe still not comprehensive, will need to check.
allTopLevelSchemas :: Traversable r => Traversal' (Root r) (Schema r)
allTopLevelSchemas fa Root{..} = (\c p -> Root rootOpenapi c rootInfo p rootSecurity rootServers rootTags rootExternalDocs)
  <$> componentTopLevelSchemas fa rootComponents
  <*> pathsTopLevelSchemas fa rootPaths


allSchemas :: (Traversable r, Plated (Schema r)) => Traversal' (Root r) (Schema r)
allSchemas = allTopLevelSchemas . plate

allDescriptions :: Traversal' (Root r) CommonMark
allDescriptions = undefined

data GenSettings = GenSettings
  { rootModule :: ScopedIdent
  }




data ContentTypeHandler = ContentTypeHandler
  { contentTypeShorthandSuffix :: Text
  } deriving (Show, Eq)

instance ToJSON ContentTypeHandler where
  toJSON c = object [ "suffix" .= contentTypeShorthandSuffix c ]

newtype SchemaScope f = SchemaScope { fromSchemaScope :: NonEmpty (SchemaParent f) }

deriving instance (Show1 f) => Show (SchemaScope f)
deriving instance (Eq1 f) => Eq (SchemaScope f)

rootScope :: SchemaParent f -> SchemaScope f
rootScope t = SchemaScope (t NE.:| [])

type SchemaDescent r sig m = (Has (Reader (SchemaScope r)) sig m, Has Trace sig m)
type GlobalState sig m = Has (State GlobalGeneratorState) sig m

descendSchema :: (SchemaDescent r sig m) => SchemaParent r -> m a -> m a
descendSchema p = local $ \(SchemaScope ss) -> SchemaScope (p NE.<| ss)

-- TODO this needs to properly handle refs outside of the schema field
refName :: Reference a -> ScopedIdent
refName (RelativeReference r) = localScope $ UnscopedIdent $ T.toPascal $ last $ T.splitOn "/" $ T.decodeUtf8 $ fromJust $ rrFragment r
refName (AbsoluteReference r) = localScope $ UnscopedIdent $ T.toPascal $ last $ T.splitOn "/" $ T.decodeUtf8 $ fromJust $ uriFragment r

makeRefsHaskellFriendly :: ScopedIdent -> ScopedIdent
makeRefsHaskellFriendly sidents = ScopedIdent $ coerce $ NE.fromList $ concatMap (T.splitOn ".") $ NE.toList strs
  where
    strs = coerce sidents


-- TODO Need to allow multiple resolvers to 'refine' the type
newtype TypeResolvers r m = TypeResolvers
  { typeResolvers ::
      [ TypeResolvers r m -> Schema r -> m (Maybe RenderedType)
      ]
  } deriving (Semigroup, Monoid)

mkPair :: a -> b -> (a, b)
mkPair = (,)

yes :: Applicative f => Bool -> Text -> Maybe Text -> f (Maybe RenderedType)
yes b t v =
  pure $
  Just $
  dischargeNullable b $
  RenderedType $
  pure $
  annotate
    (localScope $ UnscopedIdent t)
    (standardIdent {identContextInstanceVia = v})

no :: Applicative f => f (Maybe RenderedType)
no = pure Nothing

wrap1 :: AnnotatedIdent IdentContext -> RenderedType -> RenderedType
wrap1 sid rt = rt { renderedTypeName = Node sid [renderedTypeName rt]}

wrap2 :: AnnotatedIdent IdentContext -> RenderedType -> RenderedType -> RenderedType
wrap2 sid rt1 rt2 = wrapN sid [rt1, rt2]

wrapN :: AnnotatedIdent IdentContext -> [RenderedType] -> RenderedType
wrapN sid rts = RenderedType
  { renderedTypeName = Node sid $ map renderedTypeName rts
  }

localMaybe :: ScopedIdent
localMaybe = localScope (UnscopedIdent "Maybe")

isNullable :: RenderedType -> Bool
isNullable rt = annotatedIdent (rootLabel $ renderedTypeName rt) == localMaybe

wrapMaybe :: RenderedType -> RenderedType
wrapMaybe rt = if isNullable rt
  then rt
  else wrap1 (annotate localMaybe standardIdent) rt

-- | Lift types where all child identifier nodes are "Maybe" and have no via strategy into consolidated Maybe
hoistNestedMaybes :: RenderedType -> RenderedType
hoistNestedMaybes rt = if not (null childLabels) && all (== annotatedMaybe) childLabels
  then wrapMaybe $ rt { renderedTypeName = Node (rootLabel $ renderedTypeName rt) childrenOfMaybes}
  else rt
  where
    annotatedMaybe = annotate localMaybe standardIdent
    childLabels = map rootLabel $ subForest $ renderedTypeName rt
    childrenOfMaybes = concatMap subForest $ subForest $ renderedTypeName rt

dischargeNullable :: Bool -> RenderedType -> RenderedType
dischargeNullable isNull_ rt = if isNull_
  then wrapMaybe rt
  else rt

builtInTypeResolvers ::
     forall m sig r.
     ( SchemaDescent r sig m
     , GlobalState sig m
     , Has Resolver sig m
     , RenderType r
     )
  => TypeResolvers r m
builtInTypeResolvers = TypeResolvers
  { typeResolvers =
      [ resolveAnyOf
      , resolvePrimitive
      ]
  }
  where
    resolveAnyOf rs s = if V.null (schemaAnyOf s)
      then pure Nothing
      else do
        converted <-
          V.toList <$> V.mapM (renderType rs) (schemaAnyOf s)
        pure $
          Just $ dischargeNullable (schemaNullable s) $ hoistNestedMaybes $ case converted of
            [c]      -> c
            [c1, c2] -> wrap2 (stdAnn $ localScope $ UnscopedIdent "Either") c1 c2
            _        -> wrapN (AnnotatedIdent (standardIdent { identChildFormat = TypeList }) $ localScope $ UnscopedIdent "AnyOf") converted
    resolvePrimitive :: TypeResolvers r m -> Schema r -> m (Maybe RenderedType)
    resolvePrimitive rs s = case s ^. type_ of
      Nothing -> pure Nothing
      Just t -> do
        let n = schemaNullable s
            primitiveTypeIdentifier = lookup t
              [ mkPair "integer" $ case s ^. format of
                  Nothing -> yes n "Int" Nothing
                  Just fmt -> case fmt of
                    "int32" -> yes n "Int32" Nothing
                    "int64" -> yes n "Int64" Nothing
                    "unix-time" -> yes n "POSIXTime" Nothing
                    _ -> no
              , mkPair "number" $ case s ^. format of
                  Nothing -> yes n "Scientific" Nothing
                  Just fmt -> case fmt of
                    "float" -> yes n "Float" Nothing
                    "double" -> yes n "Double" Nothing
              , mkPair "string" $ case s ^. format of
                  -- TODO this might behave oddly if the components.schemas field declares an enum as a top level thing
                  -- TODO figure out how to consolidate duplicate enums if possible
                  Nothing -> case s ^. OpenAPI.Types.enum of
                    Nothing -> yes n "Text" Nothing
                    Just vs -> do
                      s <- ask @(SchemaScope r)
                      let mvs' = traverse (parseMaybe parseJSON) vs
                      case mvs' of
                        Nothing -> fail "Type of field is a string, so enum types should also be text"
                        Just vs' -> do
                          let (SchemaScope ne) = s
                              (immediateParent, mRest) = NE.uncons ne
                              mNextParent = do
                                (next NE.:| _) <- mRest
                                pure next
                              namer n = case n of
                                SchemaParentComponents fullName _ -> fullName
                                SchemaParentParameter paramName _ -> paramName
                                SchemaParentSchemaProperty fullName _ -> fullName
                                _ -> ""
                              enumName =
                                (maybe "" (T.toPascal . namer) mNextParent) <>
                                (fromUnscopedIdent $ scopedToLocal $ rewriteScoped T.toPascal T.toPascal $ scopedFromDots $ namer immediateParent) <>
                                "Enum"
                              newEnum = FieldEnum
                                { fieldEnumName = localScope $ UnscopedIdent enumName
                                , fieldEnumOriginal = namer immediateParent
                                , fieldEnumOptions = fmap (\s -> EnumOption (enumName <> T.toPascal s) s) vs'
                                }
                          modify $ \gs -> (gs :: GlobalGeneratorState) & declaredEnums %~ insertIdent (localScope $ UnscopedIdent enumName) newEnum
                          pure $ Just $ RenderedType $ pure $ stdAnn (localScope $ UnscopedIdent enumName)
                  Just fmt -> case fmt of
                    "byte" -> yes n "ByteString" (Just "Base64")
                    "binary" -> yes n "ByteString" (Just "OctetSequence")
                    "decimal" -> yes n "Scientific" (Just "DecimalString")
              , mkPair "boolean" $ yes n "Bool" Nothing
              -- TODO need to handle generation of maybs here? Not 100% sure
              , mkPair "array" $ case view items s of
                  Nothing -> fail "items field MUST be present for array type fields according to OpenAPI 3.0 specification."
                  Just i -> (Just . wrap1 (stdAnn $ ScopedIdent $ pure $ UnscopedIdent "Vector")) <$> renderType rs i
              , mkPair "object" $ yes n "Object" Nothing
              ]
        join <$> sequence primitiveTypeIdentifier


runResolvers ::
     ( SchemaDescent r sig m
     )
  => TypeResolvers r m
  -> Schema r
  -> m (Maybe RenderedType)
runResolvers rs s = firstSuccess $ map (\tr -> tr rs s) $ typeResolvers rs

class RenderType f where
  renderType :: (Has Resolver sig m, SchemaDescent f sig m) => TypeResolvers f m -> f (Schema f) -> m RenderedType

instance RenderType Unresolved where
  renderType rs (Ref ref) = pure $ RenderedType $ pure $ stdAnn $ refName ref
  renderType rs (Obj o) = do
    mResult <- runResolvers rs o
    case mResult of
      Nothing -> fail ("No registered type resolver is capable of handling the following schema: " <> show o)
      Just ok -> pure ok

instance RenderType Resolved where
  renderType rs (Resolved (Resolution mRef s)) = case mRef of
    Just ref -> pure $ RenderedType $ pure $ stdAnn $ refName ref
    Nothing -> do
      mResult <- runResolvers rs s
      case mResult of
        Nothing -> fail ("No registered type resolver is capable of handling the following schema: " <> show s)
        Just ok -> pure ok
  renderType rs (RecursiveReference ref) = pure $ RenderedType $ pure $ stdAnn $ refName ref
  {-
  renderType _ (Ref ref) = pure $ RenderedType $ pure $ stdAnn $ refName ref
  renderType rs (Obj s) = do
    mResult <- runResolvers rs s
    case mResult of
      Nothing -> fail ("No registered type resolver is capable of handling the following schema: " <> show s)
      Just ok -> pure ok
  -}

predFromPattern :: ResponseKey -> Text
predFromPattern pat = parenthesizeIt $ case pat of
  ConstStatus s -> "(== " <> show s <> ")"
  StatusPatternResponse h m l -> case (h, m, l) of
    (Digit _, Digit _, Digit _)    -> error "This should have been a ConstStatus"
    (Digit h, Digit m, Wildcard)   -> mconcat
      [ "\\x -> x < "
      , show (h * 100 + (m + 1) * 10)
      , " && x >= "
      , show (h * 100 + m * 10)
      ]
    (Digit h, Wildcard, Wildcard)  -> mconcat
      [ "\\x -> x >= "
      , show (h * 100)
      , " && x < "
      , show ((h + 1) * 100)
      ]
    (Wildcard, Wildcard, Wildcard) -> "const True"
    _ -> error ("This format of pattern doesn't seem to make sense. Please file a bug explaining what it's supposed to do if you have a use case for it. " ++ show pat)
  DefaultResponse -> "(const True)"
  where
    parenthesizeIt str = "(" <> T.pack str <> ")"
