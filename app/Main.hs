module Main where

import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Lazy as BS
import Data.Csv (encodeDefaultOrderedByName)
import Lib (allCodebooks)
import Control.Monad.Trans.Maybe

main :: IO ()
main = do
  x <- runMaybeT allCodebooks
  print $ take 5 <$> x




-- let y = NHANSEYear "2003" "2004"
-- res <- runMaybeT $ getAllDatasetsForYear y -- 2001 has problem
-- res <- allCodeBooks y
-- print res

saveAllToCSV :: IO ()
saveAllToCSV = do
  return ()