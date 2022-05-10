{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import GHC.Generics (Generic)
import Data.Csv
import Text.HTML.Scalpel (URL)

data CodebookType = Questionnaire | Demographic | Dietary | Examination | Laboratory
    deriving (Generic, Show, Read)

codeBookTypes :: [CodebookType]
codeBookTypes = [Questionnaire, Demographic, Dietary, Examination, Laboratory]

data Codebook = Codebook {
    codebookType :: CodebookType,
    years :: String,
    name :: String,
    docFile :: String,
    docFileLink :: URL,
    dataFile :: String,
    dataFileLink :: URL,
    published :: String
} deriving (Generic, Show, Read)


instance ToField CodebookType where
    toField Questionnaire = "Questionnaire"
    toField Demographic = "Demographics"
    toField Dietary = "Dietary"
    toField Examination = "Examination"
    toField Laboratory = "Laboratory"

instance FromField CodebookType


instance FromRecord Codebook
instance ToRecord Codebook
instance ToNamedRecord Codebook
instance DefaultOrdered Codebook


data Variable = Variable {
    codebooksType :: CodebookType,
    varName :: String,
    description :: String,
    codebookName :: String,
    codebookDescription :: String,
    startYear :: Int,
    endYear :: Int,
    constraints :: String
} deriving (Generic, Show, Read)

instance FromRecord Variable
instance ToRecord Variable
instance ToNamedRecord Variable
instance DefaultOrdered Variable


getAllCodebooksURL :: CodebookType -> URL
getAllCodebooksURL t = base ++ case t of 
    Questionnaire -> "Questionnaire"
    Demographic -> "Demographics"
    Dietary -> "Dietary"
    Examination -> "Examination"
    Laboratory -> "Laboratory"
    where 
        base = "https://wwwn.cdc.gov/nchs/nhanes/search/DataPage.aspx?Component="


getAllVariablesURL :: CodebookType -> URL
getAllVariablesURL t = base ++ case t of 
    Questionnaire -> "Questionnaire"
    Demographic -> "Demographics"
    Dietary -> "Dietary"
    Examination -> "Examination"
    Laboratory -> "Laboratory"
    where 
        base = "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component="