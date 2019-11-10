{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- Derived from OpenAPI spec
module Stripe.Types.Reporting where

-- TODO all of this needs to be qualified to account for generated output causing conflicts
import Control.Applicative ((<|>))
import Data.Aeson (ToJSON1(..), FromJSON1(..), toJSON1, parseJSON1, Value(..))
import Data.Data (Data(..))
import Data.Functor.Const
import qualified Data.Sum
import GHC.Generics (Generic)
import OpenAPI.Support hiding (Error(..))
import OpenAPI.Support.Stripe

-- TODO All objects going into qery string need instance of QueryValueLike based on parameter format



-- Describes: "reporting.report_run"

data ReportRun
  = ReportRun
    { reportRunId :: Id (ReportingReportRun)
    {- ^ Unique identifier for the object. -}
    , reportRunObject :: ReportingReportRunObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , reportRunLivemode :: Bool
    {- ^ Always `true`: reports can only be run on live-mode data. -}
    , reportRunCreated :: POSIXTime
    {- ^ Time at which the object was created. Measured in seconds since the Unix epoch. -}
    , reportRunParameters :: FinancialReportingFinanceReportRunRunParameters
    , reportRunReportType :: Text
    {- ^ The ID of the [report type](https://stripe.com/docs/reporting/statements/api#report-types) to run, such as `"balance.summary.1"`. -}
    , reportRunStatus :: Text
    {- ^ Status of this report run. This will be `pending` when the run is initially created.
 When the run finishes, this will be set to `succeeded` and the `result` field will be populated.
 Rarely, we may encounter an error, at which point this will be set to `failed` and the `error` field will be populated. -}
    , reportRunError :: Maybe (Text)
    {- ^ If something should go wrong during the run, a message about the failure (populated when
 `status=failed`). -}
    , reportRunResult :: Maybe (File)
    {- ^ The file object representing the result of the report run (populated when
 `status=succeeded`). -}
    , reportRunSucceededAt :: Maybe (POSIXTime)
    {- ^ Timestamp at which this run successfully finished (populated when
 `status=succeeded`). Measured in seconds since the Unix epoch. -}
    } deriving (Show, Eq, Generic)


-- Describes: "reporting.report_type"

data ReportType
  = ReportType
    { reportTypeId :: Id (ReportingReportType)
    {- ^ The [ID of the Report Type](https://stripe.com/docs/reporting/statements/api#available-report-types), such as `balance.summary.1`. -}
    , reportTypeObject :: ReportingReportTypeObjectEnum
    {- ^ String representing the object's type. Objects of the same type share the same value. -}
    , reportTypeDataAvailableEnd :: POSIXTime
    {- ^ Most recent time for which this Report Type is available. Measured in seconds since the Unix epoch. -}
    , reportTypeDataAvailableStart :: POSIXTime
    {- ^ Earliest time for which this Report Type is available. Measured in seconds since the Unix epoch. -}
    , reportTypeDefaultColumns :: Vector (Text)
    {- ^ List of column names that are included by default when this Report Type gets run. (If the Report Type doesn't support the `columns` parameter, this will be null.) -}
    , reportTypeName :: Text
    {- ^ Human-readable name of the Report Type -}
    , reportTypeUpdated :: POSIXTime
    {- ^ When this Report Type was latest updated. Measured in seconds since the Unix epoch. -}
    , reportTypeVersion :: Int
    {- ^ Version of the Report Type. Different versions report with the same ID will have the same purpose, but may take different run parameters or have different result schemas. -}
    } deriving (Show, Eq, Generic)


