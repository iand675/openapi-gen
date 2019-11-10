{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- Derived from OpenAPI spec
module Stripe.Types.DeletedRadar.Lens where

-- TODO all of this needs to be qualified to account for generated output causing conflicts
import Control.Lens.TH
import qualified Stripe.Types.DeletedRadar as Gen

makeFields ''Gen.ValueList
makeFields ''Gen.ValueListItem
