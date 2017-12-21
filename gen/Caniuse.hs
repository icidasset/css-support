module Caniuse
    ( Agent(..)
    , DataSet(..)
    , Entry(..)
    , dataUrl
    , fetchData
    , singlePointVersion
    , statisticSortFn
    ) where

import Data.Aeson as Aeson
import Data.HashMap.Strict as HashMap (HashMap)
import Flow
import Network.HTTP.Req as Req
import Prelude (read)
import Protolude

import qualified Data.Char as Char (ord, toUpper)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text


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
        , description :: Aeson.Value
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



-- ⚗️  /  Out  /  Data that will be used outside this module {public}


data DataSet =
    DataSet
        { agents :: HashMap Text Agent
        , entries :: HashMap Text Entry
        }


data Entry =
    Entry
        { description :: Text
        , keywords :: [Text]
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



-- 🥡  /  Take-out {public}


dataUrl :: Url Https
dataUrl =
    https "raw.githubusercontent.com" /: "Fyrd/caniuse/master/fulldata-json/data-2.0.json"


singlePointVersion :: Text -> Text
singlePointVersion ver =
    let
        split :: [Text]
        split =
            Text.splitOn "." ver

        firstPart :: [Text]
        firstPart =
            List.take 1 split

        secondPart :: [Text]
        secondPart =
            split
                |> List.drop 1
                |> Text.concat
                |> (: [])
                |> List.filter ((/=) "")
    in
    secondPart
        |> (++) firstPart
        |> Text.intercalate "."


statisticSortFn :: (Text, Text) -> Float
statisticSortFn tuple =
    let
        v =
            tuple
                |> fst
                |> versionWithoutRange
                |> singlePointVersion
                |> Text.unpack
    in
    case reads v :: [(Float, [Char])] of
        [(x, _)] ->
            x

        [] ->
            v
                |> map Char.ord
                |> sum
                |> realToFrac



-- Filters


{-| Only keep entries that are in at least one of the given categories.
-}
filterTemporaryEntries :: [Text] -> TemporaryEntry -> Bool
filterTemporaryEntries cats entry =
    categories (entry :: TemporaryEntry)
        |> List.intersect cats
        |> List.null
        |> not


{-| We don't need all the statistics.

NOTE: Most of the statistics are already removed
      by the `reduceStatistics` function.

This removes:
- `NotSupported` items where the browser later does support it

-}
filterOutUnnecessaryStatistics :: [(Text, Text)] -> [(Text, Text)]
filterOutUnnecessaryStatistics stats =
    if List.length stats > 1 &&
       Text.isPrefixOf "n" ((List.head .> snd) stats) then
        List.drop 1 stats
    else
        stats



-- Transformers


{-| Transform a `TemporaryEntry` into an `Entry`
-}
entryTransformer :: TemporaryEntry -> Entry
entryTransformer entry =
    Entry
        { description =
            description (entry :: TemporaryEntry)
                |> decodeJsonValue
                |> fromMaybe ""

        --
        , keywords =
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
                |> HashMap.map (List.sortOn statisticSortFn)
                |> HashMap.map reduceStatistics
                |> HashMap.map filterOutUnnecessaryStatistics
                |> HashMap.map (HashMap.fromList)
        }



-- Reducers


reduceStatistics :: [(Text, Text)] -> [(Text, Text)]
reduceStatistics =
    List.foldl
        (\acc stat ->
            if List.null acc then
                [ stat ]
            else if snd (List.last acc) /= snd stat then
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


versionWithoutRange :: Text -> Text
versionWithoutRange text =
    if Text.isInfixOf "-" text then
        text
            |> Text.splitOn "-"
            |> List.head
    else
        text
