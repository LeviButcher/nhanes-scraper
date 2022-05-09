module Types where


type URL = String
data CodebookType = Questionnaire 
    deriving (Show, Read)

data Codebook = Codebook {
    codebookType :: CodebookType,
    years :: (Int, Int),
    name :: String,
    docFile :: String,
    docFileLink :: URL,
    dataFile :: String,
    dataFileLink :: URL,
    published :: String
} deriving (Show, Read)


data Variable = Variable {
    codebooksType :: CodebookType,
    varName :: String,
    description :: String,
    codebookName :: String,
    codebookDescription :: String,
    startYear :: Int,
    endYear :: Int,
    constraints :: String
} deriving (Show, Read)


getAllCodebooksURL :: CodebookType -> URL
getAllCodebooksURL t = base ++ case t of 
    Questionnaire -> ""
    where 
        base = "https://wwwn.cdc.gov/nchs/nhanes/search/DataPage.aspx?Component="