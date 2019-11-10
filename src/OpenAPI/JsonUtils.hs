module OpenAPI.JsonUtils where
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.Types hiding (Encoding)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)

type RefMap resolution a = H.HashMap Text (resolution a)

killer :: StateT Object Parser a -> Object -> Parser a
killer p o = do
  (x, rem) <- runStateT p o
  if not (H.null rem)
    then fail $ show rem
    else pure x

require :: FromJSON a => Text -> StateT Object Parser a
require t = do
  o <- get
  r <- lift $ o .: t
  put $ H.delete t o
  pure r

require1 :: (FromJSON1 r, FromJSON a) => Text -> StateT Object Parser (r a)
require1 t = do
  o <- get
  r' <- lift $ o .: t
  r <- lift $ parseJSON1 r'
  put $ H.delete t o
  pure r

optional :: FromJSON a => Text -> StateT Object Parser (Maybe a)
optional t = do
  o <- get
  r <- lift $ o .:? t <?> Key t
  put $ H.delete t o
  pure r

optional1 :: (FromJSON1 r, FromJSON a) => Text -> StateT Object Parser (Maybe (r a ))
optional1 t = do
  o <- get
  r' <- lift $ o .:? t <?> Key t
  r <- lift $ traverse parseJSON1 r'
  put $ H.delete t o
  pure r

defaultOption :: FromJSON a => a -> Text -> StateT Object Parser a
defaultOption def t = fromMaybe def <$> optional t

defaultOption1 :: (FromJSON1 f, FromJSON a) => f a -> Text -> StateT Object Parser (f a)
defaultOption1 def t = fromMaybe def <$> optional1 t

optRefMap :: (FromJSON1 f, FromJSON a) => Text -> StateT Object Parser (RefMap f a)
optRefMap t = do
  o <- get
  r'' <- lift $ o .:? t <?> Key t
  r' <- fmap (fromMaybe H.empty) $ lift $ traverse parseJSON r''
  r <- lift $ traverse parseJSON1 r'
  put $ H.delete t o
  pure r

{-
eqLift ::
     (Eq1 fOut, Eq1 fIn, Eq1 fInmost, Eq b)
  => (fOut a -> fIn (fInmost b))
  -> fOut a
  -> fOut a
  -> Bool
eqLift f l r = liftEq eq1 (f l) (f r)
-}

takeExtensions :: StateT Object Parser (H.HashMap Text Value)
takeExtensions = do
  o <- get
  let extensions = H.filterWithKey (\k _ -> "x-" `T.isPrefixOf` k) o
  let rest = H.difference o extensions
  put rest
  return extensions
