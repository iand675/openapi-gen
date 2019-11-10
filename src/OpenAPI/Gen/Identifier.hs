module OpenAPI.Gen.Identifier where
import           Data.Aeson
import           Data.Coerce
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.Hashable (Hashable)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import           GHC.Generics (Generic)

-- | Can be function, type, whatever, if it's able to be qualified in a Haskell program
newtype UnscopedIdent = UnscopedIdent
  { fromUnscopedIdent :: Text
  } deriving ( Show
             , Eq
             , Ord
             , Hashable
             , Semigroup
             , ToJSON
             , FromJSON
             , ToJSONKey
             , FromJSONKey
             , IsString
             )

rewriteUnscoped :: (Text -> Text) -> UnscopedIdent -> UnscopedIdent
rewriteUnscoped f (UnscopedIdent i) = UnscopedIdent $ f i

newtype ScopedIdent = ScopedIdent
  { fromScopedIdent :: NE.NonEmpty UnscopedIdent
  } deriving (Show, Eq, Ord, Hashable, Semigroup)

scopedToModule :: ScopedIdent -> ModuleIdent
scopedToModule (ScopedIdent i) = ModuleIdent $ NE.toList i

rewriteScoped :: (Text -> Text) -> (Text -> Text) -> ScopedIdent -> ScopedIdent
rewriteScoped fHead fRest (ScopedIdent (x NE.:| xs)) =
  ScopedIdent ((coerce fHead) x NE.:| map (coerce fRest) xs)

localScope :: UnscopedIdent -> ScopedIdent
localScope = ScopedIdent . pure

pushScope :: UnscopedIdent -> ScopedIdent -> ScopedIdent
pushScope x (ScopedIdent xs) = coerce (x NE.<| xs)

popScope :: ScopedIdent -> (UnscopedIdent, Maybe ScopedIdent)
popScope (ScopedIdent xs) = coerce (NE.uncons xs)

splitScope :: ScopedIdent -> (UnscopedIdent, ModuleIdent)
splitScope (ScopedIdent (x NE.:| xs)) = (x, ModuleIdent xs)

scope :: ScopedIdent -> ModuleIdent
scope = ModuleIdent . NE.tail . fromScopedIdent

-- not total maybe? it's cool, don't worry about it
scopedFromDots :: Text -> ScopedIdent
scopedFromDots t = case reverse $ T.splitOn "." t of
  [] -> error "scopedFromDots: can't use an empty string"
  (x:xs) -> coerce (x NE.:| xs)

scopedToLocal :: ScopedIdent -> UnscopedIdent
scopedToLocal (ScopedIdent i) = NE.head i

-- TODO use a builder or something
qualifyScoped :: ScopedIdent -> Text
qualifyScoped (ScopedIdent ((UnscopedIdent x) NE.:| xs)) =
  foldr (\(UnscopedIdent chain) current -> chain <> "." <> current) x xs

instance ToJSON ScopedIdent where
  toJSON = toJSON . qualifyScoped

-- | A way of imitating the haskell module hierarchy-
-- anything in rootLabel is a local identifier,
-- anything in the subForest is intended to able to be used qualified elsewhere.
data IdentTree a = IdentNode
  { localItems :: !(H.HashMap UnscopedIdent a)
  , submoduleItems :: !(H.HashMap UnscopedIdent (IdentTree a))
  } deriving (Show, Eq, Generic, Functor)

instance ToJSON a => ToJSON (IdentTree a)

emptyIdentTree :: IdentTree a
emptyIdentTree = IdentNode H.empty H.empty

insertIdent :: ScopedIdent -> a -> IdentTree a -> IdentTree a
insertIdent (ScopedIdent original) v rootTree = go
  (ScopedIdent $ NE.reverse original) v rootTree
  where
    go (ScopedIdent (k NE.:| rest)) v (IdentNode r f) = case rest of
      [] -> IdentNode (H.insert k v r) f
      (s:ss) ->
        IdentNode r $
        H.alter
          (\level ->
            Just $
            go (ScopedIdent (s NE.:| ss)) v $
            fromMaybe emptyIdentTree level)
          k
          f

identInUse :: ScopedIdent -> IdentTree a -> Bool
identInUse = undefined

identList :: forall a. IdentTree a -> [(ScopedIdent, a)]
identList = map (\(k, v) -> (ScopedIdent $ NE.fromList k, v)) . go []
  where
    go :: [UnscopedIdent] -> IdentTree a -> [([UnscopedIdent], a)]
    go level IdentNode{..} =
      map (\(k, v) -> (k:level, v)) (H.toList localItems) ++
      concatMap
        (\(newLevel, t) -> go (newLevel : level) t)
        (H.toList submoduleItems)

newtype ModuleIdent = ModuleIdent { fromModuleIdent :: [UnscopedIdent] }
  deriving (Eq)

formatModuleIdent :: ModuleIdent -> Text
formatModuleIdent (ModuleIdent chain) =
  T.intercalate "." $ reverse $ coerce chain

moduleIdentPieces :: ModuleIdent -> [Text]
moduleIdentPieces = coerce . reverse . fromModuleIdent

instance Show ModuleIdent where
  show = T.unpack . formatModuleIdent
instance ToJSON ModuleIdent where
  toJSON = toJSON . formatModuleIdent

groupedIdentList :: IdentTree a -> [(ModuleIdent, [(UnscopedIdent, a)])]
groupedIdentList =
  map (\(ModuleIdent k, v) -> (ModuleIdent k, v)) .
  go (ModuleIdent [])
  where
    go :: ModuleIdent -> IdentTree a -> [(ModuleIdent, [(UnscopedIdent, a)])]
    go (ModuleIdent level) IdentNode{..} =
      let rest = concatMap
            (\(newLevel, t) -> go (ModuleIdent (newLevel : level)) t)
            (H.toList submoduleItems)
      in if H.null localItems
         then rest
         else (ModuleIdent level, H.toList localItems) : rest


identsInScope :: IdentTree a -> ModuleIdent -> H.HashMap UnscopedIdent a
identsInScope tree (ModuleIdent original) = go tree $ reverse original
  where
    go IdentNode{..} [] = localItems
    go IdentNode{..} (k:ks) = case H.lookup k submoduleItems of
      Nothing -> H.empty
      Just rest -> go rest ks
