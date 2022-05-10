{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Applicative (Alternative (many, (<|>)))
import Text.HTML.Scalpel
import Text.ParserCombinators.Parsec (GenParser, char, digit, many1, parse, string)
import Control.Monad.Trans.Except
import Types
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, Exception(..))
import Network.HTTP.Client (HttpException)

allCodebooks :: ExceptT ScraperException IO [Codebook]
allCodebooks = mconcat <$> traverse allCodebooksForType codeBookTypes


data ScraperException = HttpFailure HttpException | ParseFailure
  deriving (Show)

tryScrapeUrl :: URL -> Scraper String a -> ExceptT ScraperException IO a
tryScrapeUrl s scraper = do
  res <- withExceptT HttpFailure . ExceptT . try @HttpException $ scrapeURL s scraper
  except $ case res of
    Nothing -> Left ParseFailure
    Just x -> Right x


allCodebooksForType :: CodebookType -> ExceptT ScraperException IO [Codebook]
allCodebooksForType t = tryScrapeUrl (getAllCodebooksURL t) codebooks
  where
    codebooks :: Scraper String [Codebook]
    codebooks = chroots ("tbody" // "tr") $ inSerial parseCodebookRow

    parseCodebookRow :: SerialScraper String Codebook
    parseCodebookRow = do
      yearText <- seekNext $ text "td"
      name <- seekNext $ text "td"
      docFile <- seekNext $ text (("td" // "a"))
      stepBack $ text "td" -- Used to reposition cursor
      docFileLink <- seekNext $ attr "href" ("td" // "a") -- Need to handle if its not there
      dataFile <- seekNext $ text (("td" // "a"))
      stepBack $ text "td" -- Used to reposition cursor
      dataFileLink <- seekNext $ attr "href" ("td" // "a") -- Need to handle if its not there
      published <- seekNext $ text "td"


      let years = parse parseYear "Failed to parse Year" yearText -- TODO

      return $ Codebook {
          codebookType = t,
          years =  yearText,
          name = name,
          docFile = docFile,
          docFileLink = docFileLink,
          dataFile = dataFile,
          dataFileLink = dataFileLink,
          published = published
        }


allVariables :: ExceptT ScraperException IO [Variable]
allVariables = mconcat <$> traverse allVariablesForType codeBookTypes

allVariablesForType :: CodebookType -> ExceptT ScraperException IO [Variable]
allVariablesForType t = tryScrapeUrl (getAllVariablesURL t) variables
  where
    variables :: Scraper String [Variable]
    variables = chroots ("tbody" // "tr") $ inSerial parseVariableRow

    parseVariableRow :: SerialScraper String Variable
    parseVariableRow = do
      name <- seekNext $ text "td"
      description <- seekNext $ text "td"
      codebookName <- seekNext $ text "td"
      codebookDescription <- seekNext $ text "td"
      startYear <- seekNext $ text "td"
      endYear <- seekNext $ text "td"
      seekNext $ text "td"
      constraints <- seekNext $ text "td"

      return $ Variable {
        codebooksType = t,
        varName = name,
        description = description,
        codebookName = codebookName,
        codebookDescription = codebookDescription,
        startYear = read startYear,
        endYear = read endYear,
        constraints = constraints
      }


parseYear :: GenParser Char st (Int, Int)
parseYear = do
  string "NHANES"
  s <- many1 digit
  char '-'
  e <- many1 digit
  return $ tupleBiMap read (s, e)

  where 
    tupleBiMap f (x,y) = (f x, f y)