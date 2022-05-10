{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Csv
import GHC.Generics (Generic)
import Text.HTML.Scalpel (URL)

data CodebookType = Questionnaire | Demographic | Dietary | Examination | Laboratory
  deriving (Generic, Show, Read)

codeBookTypes :: [CodebookType]
codeBookTypes = [Questionnaire, Demographic, Dietary, Examination, Laboratory]

data Codebook = Codebook
  { codebookType :: CodebookType,
    startYear :: Int,
    endYear :: Int,
    name :: String,
    docFile :: String,
    docFileLink :: URL,
    dataFile :: String,
    dataFileLink :: URL,
    published :: String
  }
  deriving (Generic, Show, Read)

instance ToField CodebookType where
  toField Questionnaire = "Questionnaire"
  toField Demographic = "Demographics"
  toField Dietary = "Dietary"
  toField Examination = "Examination"
  toField Laboratory = "Laboratory"

instance FromField CodebookType where
  parseField = undefined

instance FromRecord Codebook

instance ToRecord Codebook

instance ToNamedRecord Codebook

instance DefaultOrdered Codebook

data Variable = Variable
  { codebooksType :: CodebookType,
    varName :: String,
    description :: String,
    codebookName :: String,
    codebookDescription :: String,
    codebookStartYear :: Int,
    codebookEndYear :: Int,
    constraints :: String
  }
  deriving (Generic, Show, Read)

instance FromRecord Variable

instance ToRecord Variable

instance ToNamedRecord Variable

instance DefaultOrdered Variable

getAllCodebooksURL :: CodebookType -> URL
getAllCodebooksURL t =
  base ++ case t of
    Questionnaire -> "Questionnaire"
    Demographic -> "Demographics"
    Dietary -> "Dietary"
    Examination -> "Examination"
    Laboratory -> "Laboratory"
  where
    base = "https://wwwn.cdc.gov/nchs/nhanes/search/DataPage.aspx?Component="

getAllVariablesURL :: CodebookType -> URL
getAllVariablesURL t =
  base ++ case t of
    Questionnaire -> "Questionnaire"
    Demographic -> "Demographics"
    Dietary -> "Dietary"
    Examination -> "Examination"
    Laboratory -> "Laboratory"
  where
    base = "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component="