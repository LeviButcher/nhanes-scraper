{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Exception (catch, try)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Functor ((<&>))
import Data.List (uncons)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import Network.HTTP.Client (HttpException (HttpExceptionRequest))
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
    seekNext,
    seekBack,
    stepBack,
    inSerial
  )
import Text.ParserCombinators.Parsec (GenParser, char, digit, many1, parse, string)
import Types
import Control.Monad.Trans.Maybe


allCodebooks :: MaybeT IO [Codebook]
allCodebooks = return []

allCodebooksForType :: CodebookType -> IO (Maybe [Codebook])
allCodebooksForType t =  scrapeURL (getAllCodebooksURL t) codebooks
  where
    codebooks :: Scraper String [Codebook]
    codebooks = chroots ("tbody" @: [] // "tr") $ inSerial parseCodebookRow

    -- parseCodebookRow :: Scraper String Codebook
    parseCodebookRow = do

      yearText <- seekNext $ text "td"
      name <- seekNext $ text "td"
      docFile <- seekNext $ text (("td" // "a"))
      stepBack $ text "td" -- Used to reposition cursor
      docFileLink <- seekNext $ attr "href" ("td" // "a") -- Unsure
      dataFile <- seekNext $ text (("td" // "a"))
      stepBack $ text "td" -- Used to reposition cursor
      dataFileLink <- seekNext $ attr "href" ("td" // "a") -- Unsure
      published <- seekNext $ text "td"


      let years = parse parseYear "Failed to parse Year" yearText

      return $ Codebook {
          codebookType = Questionnaire,
          years =  (5,5),
          name = name,
          docFile = docFile,
          docFileLink = docFileLink,
          dataFile = dataFile,
          dataFileLink = dataFileLink,
          published = published
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

-- allComments :: IO (Maybe [Comment])
-- allComments = scrapeURL "http://example.com/article.html" comments
--    where
--        comments :: Scraper String [Comment]
--        comments = chroots ("div" @: [hasClass "container"]) comment

--        comment :: Scraper String Comment
--        comment = textComment <|> imageComment

--        textComment :: Scraper String Comment
--        textComment = do
--            author      <- text $ "span" @: [hasClass "author"]
--            commentText <- text $ "div"  @: [hasClass "text"]
--            return $ TextComment author commentText

--        imageComment :: Scraper String Comment
--        imageComment = do
--            author   <- text       $ "span" @: [hasClass "author"]
--            imageURL <- attr "src" $ "img"  @: [hasClass "image"]
--            return $ ImageComment author imageURL