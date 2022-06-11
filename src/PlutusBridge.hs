module PlutusBridge (run) where


import Data.ByteString         (ByteString)
import PlutusTx                (ToData)
import PlutusBridge.SampleData
import PlutusBridge.Utils


run :: ToData a => FilePath -> [(ByteString, a)] -> IO ()
run fileName = generateModule fileName . makeSampleDataList


