module Main where

import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Lazy as BS
import Data.Csv (encodeDefaultOrderedByName)
import Lib 
import Control.Monad.Trans.Except
import Types
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Csv

-- TODO
-- [x] Codebook Data
--  [x] Save as CSV
--  [] Save as JSON
-- [x] Variable Data
--  [x] Save as CSV
--  [] Save as JSON

main :: IO ()
main = do
  res <- runExceptT app
  print res


app :: ExceptT ScraperException IO ()
app = do
  codebooks <- allCodebooks
  liftIO $ BS.writeFile "results/nhanes_codebooks.csv" (encodeDefaultOrderedByName codebooks)
  liftIO $ print codebooks

  vars <- allVariables
  liftIO $ BS.writeFile "results/nhanes_variables.csv" (encodeDefaultOrderedByName vars)
  liftIO $ print vars
  return ()