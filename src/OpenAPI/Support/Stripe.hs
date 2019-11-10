{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module OpenAPI.Support.Stripe where
import Control.Applicative
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Binary (Binary)
import Data.Data (Data)
import Data.Functor.Classes
import Data.Text (Text)
import qualified Data.Sum
import GHC.Generics (Generic)
import GHC.Exts (IsString, IsList)
import Text.Printf (PrintfArg)
import OpenAPI.Support

newtype Id (a :: k) = Id
  { fromId :: Text
  } deriving ( Show
             , Eq
             , Ord
             , Generic
             , Data
             , Read
             , IsString
             , Semigroup
             , Monoid
             , PrintfArg
             , Binary
             , NFData
             , IsList
             , ToJSON
             , FromJSON
             )

data Expandable (fs :: [* -> *])
  = Unexpanded (Id fs)
  | Expanded (AnyOf fs)

instance Data.Sum.Apply ToJSON1 fs => ToJSON (Expandable fs) where
  toJSON (Unexpanded k) = toJSON k
  toJSON (Expanded s) = toJSON1 s

instance FromJSON1 (Data.Sum.Sum fs) => FromJSON (Expandable fs) where
  parseJSON v = (Unexpanded <$> parseJSON v) <|> (Expanded <$> parseJSON1 v)
deriving instance Data.Sum.Apply Show1 a => Show (Expandable a)
deriving instance Data.Sum.Apply Eq1 a => Eq (Expandable a)
deriving instance (Data.Sum.Apply Eq1 a, Data.Sum.Apply Ord1 a) => Ord (Expandable a)
