module OpenAPI.TemplateHelpers where

import Control.Algebra
import Control.Applicative (Alternative(..), liftA2)
import Control.Carrier.Error.Either
import Control.Carrier.Fail.Either
import Control.Carrier.Throw.Either
import Control.Effect.Sum
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import qualified Data.HashMap.Strict as H
import GHC.Generics (Generic1)
import Text.EDE
import Text.EDE.Filters
import Text.Pandoc.Class
import Text.Pandoc.Extensions
import Text.Pandoc.Options (def, ReaderOptions(..), WriterOptions(..))
import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers.Haddock

import OpenAPI.Gen.Coders
import OpenAPI.Gen (predFromPattern)
import OpenAPI.Types (MT)

import Prelude hiding (fail)

instance ToJSON a => Quote (Maybe a)
instance Unquote MT


newtype TemplateLoadError = TemplateLoadError { templateLoadError :: String }
newtype TemplateRenderError = TemplateRenderError { templateRenderError :: String }

refail :: (MonadFail m) => FailC m a -> m a
refail m = do
  r <- runFail m
  case r of
    Left e -> fail e
    Right ok -> pure ok

throwToFail :: (MonadFail m, Has Fail sig m) => (e -> String) -> ErrorC e m a -> m a
throwToFail f m = do
  r <- runError m
  case r of
    Left e -> fail $ f e
    Right ok -> pure ok

data Templater m k
  = LoadTemplate FilePath (Template -> m k)
  | RenderWith (H.HashMap Id Term) Template Object (L.Text -> m k)
  deriving (Functor, Generic1)

instance Effect Templater

loadTemplate :: (Has Templater sig m) => FilePath -> m Template
loadTemplate fp = send $ LoadTemplate fp pure

renderWith :: (Has Templater sig m) => H.HashMap Id Term -> Template -> Object -> m L.Text
renderWith fs t o = send $ RenderWith fs t o pure

newtype TemplaterC m a = TemplaterC { runTemplater' :: m a }
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadFail
    , MonadFix
    , MonadIO
    , MonadPlus
    )

runTemplater ::
     (MonadFail m, Has Fail sig m)
  => TemplaterC (ErrorC TemplateRenderError (ErrorC TemplateLoadError m)) a
  -> m a
runTemplater m = throwToFail templateLoadError $ throwToFail templateRenderError $ runTemplater' m

instance MonadUnliftIO m => MonadUnliftIO (TemplaterC m) where
  askUnliftIO = TemplaterC $ withUnliftIO $ \u -> return (UnliftIO (unliftIO u . runTemplater'))
  {-# INLINE askUnliftIO #-}
  withRunInIO inner = TemplaterC $ withRunInIO $ \run -> inner (run . runTemplater')
  {-# INLINE withRunInIO #-}

instance MonadTrans TemplaterC where
  lift = TemplaterC

instance ( MonadIO m
         , Algebra sig m
         , Has (Throw TemplateLoadError) sig m
         , Has (Throw TemplateRenderError) sig m
         ) =>
         Algebra (Templater :+: sig) (TemplaterC m) where
  alg (L (LoadTemplate fp k)) = do
    r <- liftIO $ eitherParseFile fp
    either (throwError . TemplateLoadError) k r
  alg (L (RenderWith fs t o k)) =
    either (throwError . TemplateRenderError) k $ eitherRenderWith fs t o
  alg (R other) = TemplaterC $ handleCoercible other
  {-# INLINE alg #-}


makeStandardTemplateHelpers :: Coders -> H.HashMap Id Term
makeStandardTemplateHelpers coders =
  H.fromList
    [ "commonMarkToHaddock" @: commonMarkToHaddock
    , "contentTypeSuffix" @:
      (\t -> coderSuffix <$> resolveCoder coders t)

    , "patternPredicate" @: predFromPattern
    ]

commonMarkToHaddock :: Text -> Text
commonMarkToHaddock mark = case runPure (readCommonMark commonMarkDefs mark >>= writeHaddock haddockDefs) of
  Left err -> error $ show err
  Right ok -> ok
  where
    commonMarkDefs =
      def { readerExtensions = enableExtension Ext_raw_html $ getDefaultExtensions "commonmark" }
    haddockDefs =
      def { writerExtensions = getDefaultExtensions "haddock" }
