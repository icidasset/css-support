module Elm where

import Data.HashMap.Strict (HashMap, (!))
import Flow
import NeatInterpolation
import Protolude

import qualified Cases
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text


-- Internal


import Caniuse (Agent(..), DataSet(..), Entry(..))



-- 🍯


createModules :: DataSet -> IO ()
createModules set = do
    writeFile
        "src/Rules.elm"
        (elmModule "Rules" set)



-- Elm module


elmModule :: Text -> DataSet -> Text
elmModule name set =
    let
        agentsMap =
            (agents set)

        browsers =
            agentsMap
                |> HashMap.toList
                |> map snd
                |> map browser
                |> map browserUnionValue
                |> List.sort
                |> prepUnionValues
                |> Text.intercalate "\n"

        functions =
            (entries set)
                |> HashMap.toList
                |> List.sortOn fst
                |> map (elmEntry agentsMap)
                |> Text.intercalate "\n\n\n"
    in
    [text|
        module ${name} exposing (..)

        {-| Documentation goes here.

        # A
        @docs ...

        -}


        -- Types


        type Supported
            = Supported
            | SupportedWithPrefix
            | PartiallySupported String
            | PartiallySupportedWithPrefix String
            | NotSupported


        type Browser
            $browsers


        type alias BrowserSupport =
            { browser : Browser
            , support : Supported
            , version : Float
            }


        type alias Target =
            ( Browser, Float )



        -- Functions


        $functions
    |]



-- Entries


elmEntry :: HashMap Text Agent -> (Text, Entry) -> Text
elmEntry agents (key, entry) =
    let
        fnName =
            Cases.camelize key

        statistics =
            (stats entry)
                |> HashMap.toList
                |> List.sortOn fst
                |> map (elmStat agents)
                |> prepListValues
                |> Text.intercalate "\n"
    in
    [text|
        $fnName =
            $statistics
            ]
    |]


elmStat :: HashMap Text Agent -> (Text, HashMap Text Text) -> Text
elmStat agents (key, statistic) =
    let
        browserValue =
            agents
                |> HashMap.lookup key
                |> map browser
                |> map browserUnionValue
                |> fromMaybe key
    in
    [text|
        { browser = $browserValue
        , support = TODO
        , version = TODO
        }
    |]



-- Building Elm Stuff


browserUnionValue :: Text -> Text
browserUnionValue browserName =
    browserName
        |> Text.replace " " "_"
        |> Text.toLower
        |> Cases.camelize
        |> modulize


modulize :: Text -> Text
modulize text =
    Text.append
        (Text.take 1 text |> Text.toUpper)
        (Text.drop 1 text)


prepValues :: Char -> Char -> [Text] -> [Text]
prepValues startingChar defaultChar list =
    let
        endIndex =
            length list - 1
    in
    list
        |> zip [0..endIndex]
        |> map
            (\(idx, v) ->
                if idx == 0 then
                    Text.concat [ Text.singleton startingChar, " ", v ]
                else
                    Text.concat [ Text.singleton defaultChar, " ", v ]
        )


prepListValues :: [Text] -> [Text]
prepListValues =
    prepValues '[' ','


prepUnionValues :: [Text] -> [Text]
prepUnionValues =
    prepValues '=' '|'
