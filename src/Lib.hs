{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Exception (Exception (..), try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, withExceptT)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Network.HTTP.Client (HttpException)
import Text.HTML.Scalpel
import Text.Parsec.Char (anyChar, crlf, space, tab)
import Text.ParserCombinators.Parsec (GenParser, char, count, digit, many1, manyTill, parse, string)
import Types

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
      docFile <- seekNext $ text ("td" // "a")
      stepBack $ text "td" -- Used to reposition cursor
      docFileLink <- stepNext (attr "href" ("td" // "a")) <|> return "No Link"
      dataFile <- seekNext (text ("td" // "a")) <|> seekNext (text "td")
      stepBack $ text "td" -- Used to reposition cursor
      dataFileLink <- stepNext (attr "href" ("td" // "a")) <|> return "No Link"
      published <- seekNext $ text "td"

      let (start, end) = fromRight (-1, -1) $ parse parseYear "Failed to parse Year" yearText

      return $
        Codebook
          { codebookType = t,
            startYear = start,
            endYear = end,
            name = name,
            docFile = trim docFile,
            docFileLink = docFileLink,
            dataFile = fromRight "" $ parse parseDataFile "Failed To Parse Data File" (trim dataFile),
            dataFileLink = dataFileLink,
            published = trim published
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

      return $
        Variable
          { codebooksType = t,
            varName = trim name,
            description = trim description,
            codebookName = trim codebookName,
            codebookDescription = trim codebookDescription,
            codebookStartYear = read startYear,
            codebookEndYear = read endYear,
            constraints = constraints
          }

-- This should remove any "/n/t" at start and end of string
trim :: String -> String
trim s = fromRight s $ parse parseTrimmer "Failed to trim" s

parseTrimmer :: GenParser Char st String
parseTrimmer = do
  centeringChars
  manyTill anyChar centeringChars

centeringChars :: GenParser Char st String
centeringChars = crlf >> many1 tab

parseYear :: GenParser Char st (Int, Int)
parseYear = do
  centeringChars
  s <- many1 digit
  char '-'
  e <- many1 digit

  return $ bimap read read (s, e)

parseDataFile :: GenParser Char st String
parseDataFile = manyTill anyChar space