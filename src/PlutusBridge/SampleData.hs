{-# LANGUAGE RecordWildCards   #-}


module PlutusBridge.SampleData
  ( SampleData
  , makeSampleData
  , makeSampleData'
  , makeSampleDataList
  , makeSampleDataList'
  , getFunctionName
  , getData
  ) where


import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           PlutusTx        (Data, ToData, toData)


data SampleData = SampleData
  { sdFunctionName :: ByteString
  , sdData         :: Data
  }


makeSampleData :: ToData a => ByteString -> a -> SampleData
makeSampleData fnName customData =
  SampleData
    { sdFunctionName = fnName
    , sdData         = toData customData
    }


makeSampleData' :: ByteString -> Data -> SampleData
makeSampleData' fnName customData =
  SampleData
    { sdFunctionName = fnName
    , sdData         = customData
    }


makeSampleDataList :: ToData a => [(ByteString, a)] -> [SampleData]
makeSampleDataList = map (uncurry makeSampleData)


makeSampleDataList' :: [(ByteString, Data)] -> [SampleData]
makeSampleDataList' = map (uncurry makeSampleData')


getFunctionName :: SampleData -> ByteString
getFunctionName SampleData {..} = sdFunctionName


getData :: SampleData -> Data
getData SampleData {..} = sdData
