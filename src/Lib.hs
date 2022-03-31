{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified CSV as C
import Control.Applicative (Alternative (many))
import Control.Exception (try)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Functor ((<&>))
import Data.List (uncons)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import Text.HTML.Scalpel
  ( Scraper,
    Selector,
    URL,
    anySelector,
    attr,
    chroots,
    hasClass,
    scrapeURL,
    text,
    texts,
    (//),
    (@:),
    (@=),
  )
import Text.ParserCombinators.Parsec (GenParser, char, digit, many1, parse, string)

data Feature = Feature
  { label :: String,
    description :: String
  }
  deriving (Show)

data NHANSEDataset = NHANSEDataset
  { year :: NHANSEYear,
    dataset :: [(CodeBook, [Feature])]
  }
  deriving (Show)

data CodeBook = CodeBook
  { name :: String,
    url :: String,
    codebookType :: C.CodeBookType
  }
  deriving (Show)

data NHANSEYear = NHANSEYear
  { startYear :: String,
    endYear :: String
  }
  deriving (Show)

nhanseDatasetToCSVRecords :: NHANSEDataset -> [C.NHANSERecord]
nhanseDatasetToCSVRecords x = records
  where
    y = year x
    set = dataset x
    records =
      set
        >>= ( \(codeB, features) ->
                features <&> \f ->
                  C.NHANSERecord
                    { C.startYear = startYear y,
                      C.endYear = endYear y,
                      C.codebookName = name codeB,
                      C.label = label f,
                      C.description = description f,
                      C.codebookType = show $ codebookType codeB
                    }
            )

getAllYearsDatasets :: MaybeT IO [NHANSEDataset]
getAllYearsDatasets = do
  years <- MaybeT allYears
  traverse getAllDatasetForYear years

getAllDatasetForYear :: NHANSEYear -> MaybeT IO NHANSEDataset
getAllDatasetForYear year = do
  allCodeBooks <- MaybeT $ allCodeBooks year
  let codeUrls = getFullCodeBookURL <$> allCodeBooks
  features <- MaybeT $ sequence <$> traverse allFeatures codeUrls

  return $ NHANSEDataset year (zip allCodeBooks features)

getFullCodeBookURL :: CodeBook -> String
getFullCodeBookURL y = "https://wwwn.cdc.gov" ++ url y

allFeatures :: URL -> IO (Maybe [Feature])
allFeatures url = scrapeURL url features
  where
    features :: Scraper String [Feature]
    features = chroots ("div" @: ["class" @= "pagebreak"]) parseFeature

    parseFeature :: Scraper String Feature
    parseFeature = do
      [label, description] <- texts $ "dd" @: [hasClass "info"]
      return $ Feature label description

getCodeBooks :: NHANSEYear -> C.CodeBookType -> IO (Maybe [CodeBook])
getCodeBooks nYear cType = scrapeURL (getCodeBooksURL cType nYear) codebooks
  where
    codebooks :: Scraper String [CodeBook]
    codebooks = chroots ("table" @: ["id" @= "GridView1"] // "tbody" // "tr") codebook

    codebook :: Scraper String CodeBook
    codebook = do
      name <- text $ "td" @: [hasClass "text-left"]
      url <- attr "href" ("td" @: [hasClass "text-center"] // "a")
      return $ CodeBook name url cType

allCodeBooks :: NHANSEYear -> IO (Maybe [CodeBook])
allCodeBooks nYear = fmap join . sequence <$> traverse (getCodeBooks nYear) C.allCodeBookTypes

baseCodebook :: String
baseCodebook = "https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component="

getCodeBooksURL :: C.CodeBookType -> NHANSEYear -> URL
getCodeBooksURL cType y = baseCodebook ++ typeParam cType ++ "&CycleBeginYear=" ++ startYear y
  where
    typeParam C.Dietary = "Dietary"
    typeParam C.Demographics = "Demographics"
    typeParam C.Examination = "Examination"
    typeParam C.Laboratory = "Laboratory"
    typeParam C.Questionnaire = "Questionnaire"

nhanseYearSelector :: Selector
nhanseYearSelector = "main" // "div" @: [hasClass "row"] // "div" @: [hasClass "card-title"]

allYears :: IO (Maybe [NHANSEYear])
allYears = scrapeURL "https://wwwn.cdc.gov/nchs/nhanes/Default.aspx" years
  where
    years :: Scraper String [NHANSEYear]
    years = catMaybes <$> chroots nhanseYearSelector year

    year :: Scraper String (Maybe NHANSEYear)
    year = do
      x <- text anySelector
      let res = parse parseNhanseYear "Failed to parser" x

      return $ either (const Nothing) Just res

parseNhanseYear :: GenParser Char st NHANSEYear
parseNhanseYear = do
  string "NHANES"
  s <- many1 digit
  char '-'
  e <- many1 digit
  return $ NHANSEYear s e