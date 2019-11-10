{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- Derived from OpenAPI spec
module Stripe.Types.Issuing.Lens where

-- TODO all of this needs to be qualified to account for generated output causing conflicts
import Control.Lens.TH
import qualified Stripe.Types.Issuing as Gen

makeFields ''Gen.Authorization
makeFields ''Gen.Card
makeFields ''Gen.CardDetails
makeFields ''Gen.CardPin
makeFields ''Gen.Cardholder
makeFields ''Gen.Dispute
makeFields ''Gen.Settlement
makeFields ''Gen.Transaction
makeFields ''Gen.Verification
