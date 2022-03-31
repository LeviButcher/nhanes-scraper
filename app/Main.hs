module Main where

import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Lazy as BS
import Data.Csv (encodeDefaultOrderedByName)
import Lib (NHANSEYear (NHANSEYear), allCodeBooks, allYears, getAllYearsDatasets, nhanseDatasetToCSVRecords)

main :: IO ()
main = do
  -- res <- allYears
  -- let x = NHANSEYear "1999" "2000"
  -- res <- allCodeBooks x

  -- print res

  res <- runMaybeT getAllYearsDatasets
  case res of
    Nothing -> putStrLn "Something failed boss"
    Just x -> do
      let res = x >>= nhanseDatasetToCSVRecords
      BS.writeFile "nhanse_features.csv" (encodeDefaultOrderedByName res)
  return ()
