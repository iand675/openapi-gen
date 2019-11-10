module OpenAPI.Gen.Language.Haskell where

import Control.Applicative hiding (optional)
import qualified Control.Applicative as App
import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Fail.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Throw.Either
import Control.Carrier.Trace.Printing
import Control.Lens hiding (view, use, assign, (.=), (<.>))
import Control.Effect.Lens (view, use, assign)
import Control.Monad hiding (fail)
import Control.Monad.Trans
import Control.Lens.TH
import Data.Aeson hiding (Encoding)
import Data.Aeson.Text
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Attoparsec.Text as P
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as M
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Manipulate as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.Traversable
import qualified Data.Vector as V
import GHC.Generics (Generic, Generic1)
import Prelude hiding (fail)
import System.Directory
import System.FilePath
import System.Timeout
import Text.EDE hiding (renderWith)
import Text.EDE.Filters

import OpenAPI.Gen
import OpenAPI.Gen.Config
import OpenAPI.Gen.Coders
import OpenAPI.Gen.Identifier
import OpenAPI.Gen.Stripe
import OpenAPI.Gen.Writer
import OpenAPI.TemplateHelpers
import OpenAPI.Types
import OpenAPI.Resolve ()
import qualified OpenAPI.Resolve as R

-- TODO provide a means of resolving references instead of just munging names
type RootSig r sig m =
  ( Has (State (Root r)) sig m
  , Has (State GlobalGeneratorState) sig m
  , Has Fail sig m
  , Has Trace sig m
  , Has R.Resolver sig m
  )

traverseComponentSchemas :: forall sig m r a. (MonadFail m, RootSig Unresolved sig m) => (Text -> Schema Unresolved -> m a) -> m [a]
traverseComponentSchemas f = do
  r <- get @(Root Unresolved)
  forM (r ^. (components . schemas . to H.toList)) $ \(k, v) -> case v of
    Ref r -> fail ("Top-level references in the components object not currently supported. Please file a ticket if you need this: " <> show r)
    Obj o -> f k o

traversePaths :: (RootSig r sig m) => (Path -> H.HashMap Method (ApiEndpoint r) -> m a) -> m [a]
traversePaths f = do
  Root{..} <- get
  let pathList = H.toList rootPaths
  forM pathList $ \(k, v) -> f k v


withPath ::
     (RootSig r sig m)
  => Path
  -> H.HashMap Method (ApiEndpoint r)
  -> ReaderC Path (ReaderC (H.HashMap Method (ApiEndpoint r)) m) a
  -> m a
withPath p methods m = runReader methods $ runReader p m

data TypeOutputModule
  = TypeOutputModule [Text]
  deriving (Show)

unusableKeywords :: S.Set Text
unusableKeywords = S.fromList
  [ "type"
  , "newtype"
  , "data"
  , "class"
  , "instance"
  , "case"
  , "of"
  , "module"
  , "import"
  , "qualified"
  , "where"
  , "as"
  , "foreign"
  , "hiding"
  , "if"
  , "then"
  , "else"
  , "infixl"
  , "infix"
  , "infixr"
  , "let"
  , "in"
  ]

renameConflictingFields ::
     Text -- ^ Original field name
  -> Text -- ^ Record field name
  -> Text -- ^ Fixed record field name
renameConflictingFields original t = if S.member original unusableKeywords
  then t <> "_"
  else t

renderComponentSchemaDataTypes ::
     (MonadFail m, RootSig Unresolved sig m)
  => TypeResolvers Unresolved (ReaderC (SchemaScope Unresolved) m)
  -> m ()
renderComponentSchemaDataTypes resolvers = do
  modify $ over (allSchemas @Unresolved) annotateSchemaWithEnumFromDescription

  r <- get @(Root Unresolved)
  traverseComponentSchemas $ \n fs -> runReader (rootScope $ SchemaParentComponents n $ rootComponents r) $ do
    let raw = scopedFromDots n
        cfn = rewriteScoped T.toPascal T.toPascal raw
    ctorFields <- sequence $
      map (\(k, v) -> descendSchema (SchemaParentSchemaProperty k fs) $ constructorFieldFromSchemaProperty raw (k, v)) $
      H.toList $
      schemaProperties fs
    let r = DataType
              { typeName = fromUnscopedIdent $ scopedToLocal cfn
              , typeOriginalName = n
              , typeDescription = fromCommonMark <$> schemaDescription fs
              , constructors =
                [ Constructor
                  { constructorName = fromUnscopedIdent $ scopedToLocal cfn
                  , constructorFields = sortOn fieldOrderProjection ctorFields
                  }
                ]
              , codings = includeCoders standardCoders [MT "application/json"]
              , instances = []
              }
    modify $ \rs -> (rs :: GlobalGeneratorState) & declaredTypes %~ insertIdent cfn r
    pure r
  return ()
  -- TODO component requests
  -- TODO component responses
  -- TODO endpoint requests
  -- TODO endpoint responses

  where
    constructorFieldFromSchemaProperty rawTypeName (n, r) = do
      t <- renderType resolvers r
      pure $ ConstructorField
        { fieldName = renameConflictingFields n $ T.toCamel (fromUnscopedIdent $ scopedToLocal rawTypeName) <> T.toPascal n
        , fieldOriginalName = n
        , fieldType = T.pack $ show t
        , fieldNullable = isNullable t
        , fieldDescription = case r of
            Ref _ -> Nothing
            Obj s -> fromCommonMark <$> schemaDescription s
        }

renderApiEndpoints ::
     ( MonadFail m
     , RootSig Unresolved sig m
     )
  => TypeResolvers Unresolved (ReaderC (SchemaScope Unresolved) m) -> m ()
renderApiEndpoints resolvers = do
  result <- traversePaths $ \route methods ->
    forM (H.toList methods) $ \(method, ApiEndpoint{..}) -> do
      typeResolvedParams <- forM apiEndpointParameters $ \param -> case parameterSchema  param of
        Nothing -> fail "Parameters using the 'content' field are not yet supported. Please file an issue if you need this."
        Just rs -> do
          t <- runReader (rootScope $ SchemaParentParameter (parameterName param) param) $
               renderType resolvers {- (parameterName param) -} rs
          pure $ TypedParameter t param

      --- TODO combine into one pass
      let pathParams =
            V.filter ((== ParameterInPath) . parameterIn_) apiEndpointParameters
          queryParams =
            V.filter ((== ParameterInQuery) . parameterIn_) apiEndpointParameters
          headerParams =
            V.filter ((== ParameterInHeader) . parameterIn_) apiEndpointParameters
          cookieParams =
            V.filter ((== ParameterInCookie) . parameterIn_) apiEndpointParameters

      pathPieces <- forM (pathSegments route) $ \piece -> case piece of
        ConstSegment _ -> pure $ ResolvedPathSegment piece Nothing
        NamedSegment n -> case find (\p -> parameterName p == n) pathParams of
          Nothing -> fail ("Unable to locate parameter " <> show n <> " for path " <> show piece)
          Just found -> pure $ ResolvedPathSegment piece $ Just found

      let nestedLists =
            H.toList $
            fmap (\r -> M.toList $ responseContent r) $
            (apiEndpointResponses ^. responses)
          searchSpace = do
            (pat, resps) <- nestedLists
            (mediaTy, ref) <- resps
            -- TODO need to resolve this
            mt <- toList ref
            -- TODO need to handle encoding
            s <- toList $ mediaTypeSchema mt
            pure (s, mt, \rt -> OpenAPI.Gen.Response
                                { responseContentType = mediaTy
                                , responsePattern = pat
                                , responsePatternPredicate = predFromPattern pat
                                , responseSuffix = prettyResponseSuffix (Just mediaTy) pat
                                , responseType = rt
                                , responseDecoderFunction = "noodle"
                                })
      flatResponses <- forM searchSpace $ \(s, mt, rf) ->
        runReader (rootScope $ SchemaParentMediaType mt) (rf <$> renderType resolvers s)

      -- We need [(status, [(contentType, {status, content, suffix})])]
      let groupedByPatResponses = groupBy (\r1 r2 -> responsePattern r1 == responsePattern r2) flatResponses
      -- Unsafe, but meh.
          nestedResponses = H.fromList $ map (\l -> (responsePattern $ head l, NestedResponse apiEndpointOperationId (M.fromList $ map (\r -> (responseContentType r, r)) l) [])) groupedByPatResponses

      pure $ Endpoint
        { endpointMethod = method
        , endpointDescription = apiEndpointDescription
        , endpointName = apiEndpointOperationId
        , endpointPath = route
        , endpointDeprecated = apiEndpointDeprecated
        , endpointParameters = typeResolvedParams
        , endpointRequestBodyRequired = requestBodyRequired apiEndpointRequestBody
        , endpointRequestBodies = requestBodyContent apiEndpointRequestBody
        , endpointResolvedPath = pathPieces
        , endpointResolvedQuery = queryParams
        , endpointResolvedHeaders = headerParams
        , endpointResolvedCookie = cookieParams
        , endpointFlatResponses = flatResponses
        , endpointNestedResponses = nestedResponses
        }
  assign @GlobalGeneratorState declaredEndpoints $ concat result



-- TODO add support for custom adjustedTypes + patching over types with more specific values
go :: HaskellConfig -> (Value, Root Unresolved) -> IO ()
go conf (rVal, r) = refail $ runTrace $ runTemplater $ do
  trace "Start"
  let resolvers = (stripeEnhancedResolvers <> builtInTypeResolvers)
  res <-
    runFail $
    throwToFail (\e -> show (e :: R.JsonError)) $
    throwToFail (\e -> show (e :: R.RemoteReferencesUnsupported)) $
    R.runLocalResolver rVal $ do
      execState emptyGlobalGeneratorState $ evalState r $ do
        renderComponentSchemaDataTypes resolvers
        renderApiEndpoints resolvers

  trace "Groups"
  gs <- either fail pure res
  trace "Load template"
  typeTemplate <- loadTemplate "template/types.ede"
  typeLensTemplate <- loadTemplate "template/types.lens.ede"
  forM_ (gs ^. declaredTypes . to groupedIdentList) $ \(modulePath, types) -> do
    trace ("Process module " <> show modulePath)
    let basePath = ["Stripe", "Types"]
        modulePath' = ModuleIdent $ (++ reverse basePath) $ fromModuleIdent modulePath
        filePath = "src" </> (T.unpack $ T.intercalate "/" $ moduleIdentPieces modulePath') <.> "hs"
        lensPath = "src" </> (T.unpack $ T.intercalate "/" $ moduleIdentPieces modulePath') </> "Lens" <.> "hs"
    liftIO $ createDirectoryIfMissing True (takeDirectory filePath)
    liftIO $ createDirectoryIfMissing True (takeDirectory lensPath)
    let env =
          fromPairs
            [ "types" .= map snd (sortOn fst types)
            , "enums" .= map snd (sortOn fst $ H.toList $ identsInScope (gs ^. declaredEnums) modulePath)
            -- , "endpoints" .= (gs ^. declaredEndpoints)
            , "moduleName" .= modulePath'
            ]
    liftIO $ BL.writeFile (show modulePath' <> ".json") $ encodePretty env
    either error (liftIO . L.writeFile filePath) $ eitherRenderWith customFilters typeTemplate env
    either error (liftIO . L.writeFile lensPath) $ eitherRenderWith customFilters typeLensTemplate env

  trace "Endpoints module"
  liftIO $ BL.writeFile "Stripe.Endpoints.json" (gs ^. declaredEndpoints . to encodePretty)
  liftIO $ BL.writeFile "Stripe.Endpoints.json" (gs ^. declaredEndpoints . to encodePretty)

  routeTemplate <- loadTemplate "template/routes.ede"
  let env = fromPairs [ "endpoints" .= (gs ^. declaredEndpoints) ]
  renderWith customFilters routeTemplate env >>= liftIO . L.writeFile "src/Stripe/Endpoints.hs"

{-
  either error (L.writeFile "src/Stripe/Types.hs") $
    typeTemplate >>= (\t -> eitherRenderWith customFilters t env)
-}
  where
    customFilters = makeStandardTemplateHelpers standardCoders
