module Mdn
    ( dataUrl
    , fetchData
    ) where

import Data.Aeson (FromJSON)
import Data.HashMap.Strict as HashMap (HashMap)
import Flow
import Network.HTTP.Req as Req
import Protolude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List (sort)


-- 🍯


fetchData :: IO [Text]
fetchData = do
    -- Make request
    json <- req GET dataUrl NoReqBody jsonResponse mempty

    -- Transform response
    let hashmap = responseBody json :: HashMap Text CssProperty

    -- Only keep standard css properties
    -- (ie. no experimental or vendor-prefixed properties)
    hashmap
        |> HashMap.filter (\p -> status p == "standard")
        |> HashMap.keys
        |> List.sort
        |> return



-- ⚗️


instance MonadHttp IO where
    handleHttpException = throwIO


data CssProperty =
    CssProperty
        { status :: Text }


deriving instance Generic CssProperty
instance FromJSON CssProperty



-- 🥡  /  Take-out {public}


dataUrl :: Url Https
dataUrl =
    https "raw.githubusercontent.com" /: "mdn/data/master/css/properties.json"
