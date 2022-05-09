module Main where

import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Lazy as BS
import Data.Csv (encodeDefaultOrderedByName)
import Lib (NHANSEYear (NHANSEYear), allCodeBooks, allYears, getAllDatasetsForAllYears, getAllDatasetsForYear, nhanseDatasetToCSVRecords)

main :: IO ()
main = do
  saveAllToCSV

-- let y = NHANSEYear "2003" "2004"
-- res <- runMaybeT $ getAllDatasetsForYear y -- 2001 has problem
-- res <- allCodeBooks y
-- print res

saveAllToCSV :: IO ()
saveAllToCSV = do
  res <- runMaybeT getAllDatasetsForAllYears
  case res of
    Nothing -> putStrLn "Something failed boss"
    Just x -> do
      let res = x >>= nhanseDatasetToCSVRecords
      BS.writeFile "results/nhanse_features.csv" (encodeDefaultOrderedByName res)
  return ()