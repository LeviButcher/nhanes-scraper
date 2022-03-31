{-# LANGUAGE DeriveGeneric #-}

module CSV where

import Data.Csv (DefaultOrdered, FromNamedRecord, ToNamedRecord)
import GHC.Generics (Generic)

data NHANSERecord = NHANSERecord
  { startYear :: String,
    endYear :: String,
    codebookName :: String,
    label :: String,
    description :: String,
    codebookType :: String
  }
  deriving (Show, Generic)

data CodeBookType = Demographics | Dietary | Examination | Laboratory | Questionnaire
  deriving (Show, Enum, Bounded)

allCodeBookTypes :: [CodeBookType]
allCodeBookTypes = [minBound ..]

instance FromNamedRecord NHANSERecord

instance ToNamedRecord NHANSERecord

instance DefaultOrdered NHANSERecord