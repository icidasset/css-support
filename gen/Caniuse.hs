module Caniuse
    ( Agent(..)
    , DataSet(..)
    , Entry(..)
    , fetchData
    ) where

import Data.Aeson as Aeson
import Data.HashMap.Strict as HashMap (HashMap)
import Flow
import Network.HTTP.Req as Req
import Protolude

import qualified Data.Char as Char (toUpper)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text (splitOn)


-- 🍯


fetchData :: IO DataSet
fetchData = do
    -- Make request
    json <- req GET dataUrl NoReqBody jsonResponse mempty

    -- Transform response
    let temporarySet    = responseBody json :: TemporaryDataSet
    let cats            = css (_cats temporarySet)

    -- Create final DataSet
    return DataSet
        { agents =
            retrieveAgents temporarySet
        , entries =
            temporarySet
                |> _entries
                |> HashMap.filter (filterTemporaryEntries cats)
                |> HashMap.map entryTransformer
        }



-- ⚗️


instance MonadHttp IO where
    handleHttpException = throwIO



-- ⚗️  /  In  /  Temporary data containers


data TemporaryDataSet =
    TemporaryDataSet
        { _agents :: Value
        , _cats :: Cats
        , _entries :: HashMap Text TemporaryEntry
        }


data TemporaryEntry =
    TemporaryEntry
        { categories :: [Text]
        , keywords :: Aeson.Value
        , notes :: Text
        , notesByNum :: Aeson.Value
        , stats :: Aeson.Value
        }


data Cats =
    Cats
        { css :: [Text]
        }


deriving instance Generic TemporaryDataSet
deriving instance Generic TemporaryEntry
deriving instance Generic Cats


instance FromJSON TemporaryDataSet where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dataSetFieldModifier }


instance FromJSON TemporaryEntry where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }


instance FromJSON Cats where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = map Char.toUpper }


dataSetFieldModifier :: [Char] -> [Char]
dataSetFieldModifier chars =
    case drop 1 chars of
        "entries" ->
            "data"

        v ->
            v



-- ⚗️  /  Out  /  Data that will be used outside this module


data DataSet =
    DataSet
        { agents :: HashMap Text Agent
        , entries :: HashMap Text Entry
        }


data Entry =
    Entry
        { keywords :: [Text]
        , notes :: Text
        , notesByNum :: HashMap Text Text
        , stats :: HashMap Text (HashMap Text Text)
        }


data Agent =
    Agent
        { browser :: Text
        , prefix :: Text
        , typ :: Text
        }


deriving instance Generic Agent
deriving instance Show DataSet
deriving instance Show Entry
deriving instance Show Agent


instance FromJSON Agent where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = agentFieldModifier }


agentFieldModifier :: [Char] -> [Char]
agentFieldModifier chars =
    if chars == "typ" then
        "type"
    else
        chars



-- 🥡


dataUrl :: Url Https
dataUrl =
    https "raw.githubusercontent.com" /: "Fyrd/caniuse/master/fulldata-json/data-2.0.json"



-- Filters


{-| Only keep entries that are in at least one of the given categories.
-}
filterTemporaryEntries :: [Text] -> TemporaryEntry -> Bool
filterTemporaryEntries cats entry =
    categories (entry :: TemporaryEntry)
        |> List.intersect cats
        |> List.null
        |> not



-- Transformers


{-| Transform a `TemporaryEntry` into an `Entry`
-}
entryTransformer :: TemporaryEntry -> Entry
entryTransformer entry =
    Entry
        { keywords =
            keywords (entry :: TemporaryEntry)
                |> decodeJsonValue
                |> map (Text.splitOn ",")
                |> fromMaybe []

        --
        , notes =
            notes (entry :: TemporaryEntry)

        --
        , notesByNum =
            notesByNum (entry :: TemporaryEntry)
                |> decodeJsonValue
                |> fromMaybe HashMap.empty

        --
        , stats =
            stats (entry :: TemporaryEntry)
                |> decodeJsonValue
                |> fromMaybe HashMap.empty
                |> HashMap.map HashMap.toList
                |> HashMap.map (List.sortOn fst)
                |> HashMap.map reduceStatistics
                |> HashMap.map (HashMap.fromList)
        }



-- Reducers


reduceStatistics :: [(Text, Text)] -> [(Text, Text)]
reduceStatistics =
    List.foldl
        (\acc stat ->
            if List.null acc then
                [ stat ]
            else if snd (List.head acc) /= snd stat then
                acc ++ [ stat ]
            else
                acc
        )
        []



-- Other functions


decodeJsonValue :: FromJSON a => Aeson.Value -> Maybe a
decodeJsonValue value =
    case fromJSON value of
        Success v ->
            Just v

        Error err ->
            Nothing


retrieveAgents :: TemporaryDataSet -> HashMap Text Agent
retrieveAgents set =
    set
        |> _agents
        |> decodeJsonValue
        |> fromMaybe HashMap.empty
