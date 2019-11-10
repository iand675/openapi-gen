module IdentifierTest where

import           OpenAPI.Gen.Identifier
import qualified Data.HashMap.Strict as H
import           Data.List (nub, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

instance Arbitrary UnscopedIdent where
  arbitrary = do
    ASCIIString str <- arbitrary `suchThat`
      (\(ASCIIString s) ->
          (not $ null s) && (not $ elem '.' s))
    pure $ UnscopedIdent $ T.pack str
  shrink =
    fmap (UnscopedIdent . T.pack . getASCIIString) .
    shrink . ASCIIString . T.unpack . fromUnscopedIdent

instance Arbitrary ScopedIdent where
  arbitrary = ScopedIdent <$> ((NE.:|) <$> arbitrary <*> arbitrary)

spec_ScopedIdent_represents_hierarchy_correctly :: Spec
spec_ScopedIdent_represents_hierarchy_correctly = specify "it works" $ do
  scopedFromDots "Foo.Bar.Baz" ==
    (ScopedIdent
       (UnscopedIdent "Baz" NE.:| [UnscopedIdent "Bar", UnscopedIdent "Foo"]))

prop_dot_separated_text_roundtrips :: Property
prop_dot_separated_text_roundtrips = forAll (arbitrary `suchThat` (\s -> not (null s) && s /= ".")) $ \str ->
  qualifyScoped (scopedFromDots (T.pack str)) === T.pack str

prop_unscoped_to_scoped :: Property
prop_unscoped_to_scoped = do
  label "Roundtrip should work" $ \unscoped ->
    scopedToLocal (localScope unscoped) == unscoped

prop_identTree_root_distinct_items_all_return :: Property
prop_identTree_root_distinct_items_all_return = property $ \unscopedElems ->
  let identTree = foldr (\i t -> insertIdent (localScope i) () t)
        emptyIdentTree
        (unscopedElems :: [UnscopedIdent])
  in (sort $ H.keys $ localItems identTree) === (nub $ sort unscopedElems)

prop_scoped_ident_insert_into_identTree_returns_identical_on_identList :: Property
prop_scoped_ident_insert_into_identTree_returns_identical_on_identList =
  property $ \scopedLad ->
    identList (insertIdent scopedLad () emptyIdentTree) === [(scopedLad, ())]

prop_splitScope_works :: Property
prop_splitScope_works = property $ \scopedLad ->
  let (ident, ModuleIdent rest) = splitScope scopedLad
  in (ScopedIdent (ident NE.:| rest)) === scopedLad

prop_scoped_ident_insert_into_identTree_returns_identical_on_grouped_identList :: Property
prop_scoped_ident_insert_into_identTree_returns_identical_on_grouped_identList =
  property $ \scopedLad ->
    let (localIdent, rest) = splitScope scopedLad
        inOut = groupedIdentList $ insertIdent scopedLad () emptyIdentTree
    in inOut === [(rest, [(localIdent, ())])]
