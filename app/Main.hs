module Main where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Lazy as BS
import Data.Csv (DefaultOrdered, ToNamedRecord, encodeDefaultOrderedByName)
import Lib (ScraperException, allCodebooks, allVariables)
import Text.HTML.Scalpel (URL)

main :: IO ()
main = do
  res <- runExceptT app
  print res

app :: ExceptT ScraperException IO ()
app = do
  codebooks <- allCodebooks
  liftIO $ writeCSV "results/nhanes_codebooks.csv" codebooks

  vars <- allVariables
  liftIO $ writeCSV "results/nhanes_variables.csv" vars

  return ()

-- Could catch if file fails to save
writeCSV :: (DefaultOrdered a, ToNamedRecord a, Show a) => URL -> [a] -> IO ()
writeCSV url x = do
  print x
  BS.writeFile url (encodeDefaultOrderedByName x)