module Elm where

import Data.HashMap.Strict (HashMap, (!))
import Flow
import NeatInterpolation
import Prelude (error)
import Protolude

import qualified Cases
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text


-- Internal


import Caniuse (Agent(..), DataSet(..), Entry(..), singlePointVersion, statisticSortFn)



-- 🍯


createModules :: DataSet -> IO ()
createModules set = do
    writeFile
        "src/CssSupport/Rules.elm"
        (elmModule "CssSupport.Rules" set)



-- DataSet -> Elm module


elmModule :: Text -> DataSet -> Text
elmModule name set =
    let
        agentsMap :: HashMap Text Agent
        agentsMap =
            (agents set)

        browsers :: Text
        browsers =
            agentsMap
                |> HashMap.toList
                |> map snd
                |> map browser
                |> map browserUnionValue
                |> List.sort
                |> prepUnionValues
                |> Text.intercalate "\n"

        functions :: Text
        functions =
            (entries set)
                |> HashMap.toList
                |> List.sortOn fst
                |> map (elmEntry agentsMap)
                |> Text.intercalate "\n\n\n"

        functionNames :: Text
        functionNames =
            (entries set)
                |> HashMap.toList
                |> map fst
                |> map Cases.camelize
                |> List.sort
                |> Text.intercalate ", "

    in
    [text|
        module ${name} exposing (..)

        {-| Every CSS related item from the Can-I-use database.

        # Types
        @docs Browser, BrowserSupport, Supported, Target, Version

        # Functions
        @docs $functionNames

        -}


        -- Types


        {-|-}
        type Browser
            $browsers


        {-|-}
        type alias BrowserSupport =
            { browser : Browser
            , note : Maybe String
            , support : Supported
            , version : Version
            }


        {-|-}
        type Supported
            = Supported
            | SupportedWithPrefix
            | PartiallySupported
            | PartiallySupportedWithPrefix
            | NotSupported


        {-|-}
        type alias Target =
            ( Browser, Version )


        {-|-}
        type Version
            = VersionNumber Float
            | VersionRange Float Float
            --
            | AllVersions
            | TechnologyPreview



        -- Functions


        $functions
    |]



-- Entries -> Elm functions


elmEntry :: HashMap Text Agent -> (Text, Entry) -> Text
elmEntry agents (key, entry) =
    let
        fnName :: Text
        fnName =
            Cases.camelize key

        statistics :: Text
        statistics =
            (stats entry)
                |> HashMap.toList
                |> List.sortOn fst
                |> map (elmStatGroup agents entry)
                |> prepListValues
                |> Text.intercalate "\n"
    in
    [text|
        {-| $key
        -}
        $fnName : List BrowserSupport
        $fnName =
            $statistics
            ]
    |]


elmStatGroup :: HashMap Text Agent -> Entry -> (Text, HashMap Text Text) -> Text
elmStatGroup agents entry (key, statGroup) =
    let
        browserValue :: Text
        browserValue =
            agents
                |> HashMap.lookup key
                |> map browser
                |> map browserUnionValue
                |> fromMaybe key
    in
    statGroup
        |> HashMap.toList
        |> List.sortOn statisticSortFn
        |> map (elmStat entry browserValue)
        |> Text.intercalate "\n, "


elmStat :: Entry -> Text -> (Text, Text) -> Text
elmStat entry browserValue (key, stat) =
    let
        noteValue :: Text
        noteValue =
            case statNote entry stat of
                "" -> "Nothing"
                x  -> Text.intercalate "\n    " [ "Just \"\"\"", x, "\"\"\"" ]

        supportValue :: Text
        supportValue =
            support stat

        versionValue :: Text
        versionValue =
            version key
    in
    [text|
        { browser = $browserValue
          , note = $noteValue
          , support = $supportValue
          , version = $versionValue
          }
    |]



-- Statistics


statNote :: Entry -> Text -> Text
statNote entry stat =
    if Text.isInfixOf "#" stat then
        let
            idx =
                stat
                    |> Text.findIndex ((==) '#')
                    |> fromMaybe (-1)
                    |> (+) 1

            noteNumber =
                Text.drop idx stat
        in
            entry
                |> notesByNum
                |> HashMap.lookup noteNumber
                |> fromMaybe ""

    else
        notes entry


support :: Text -> Text
support val =
    if Text.isPrefixOf "y x" val then
        "SupportedWithPrefix"

    else if Text.isPrefixOf "y" val then
        "Supported"

    else if Text.isPrefixOf "a x" val then
        "PartiallySupportedWithPrefix"

    else if Text.isPrefixOf "a" val then
        "PartiallySupported"

    else if Text.isPrefixOf "n" val then
        "NotSupported"

    else if Text.isPrefixOf "p" val then
        "NotSupported"

    else if "u" == val then
        "NotSupported"

    else
        [text|Unhandled statistic key `$val` in the `support` function.|]
            |> Text.unpack
            |> error


version :: Text -> Text
version val =
    if Text.isInfixOf "-" val then
        let
            split :: [Text]
            split =
                Text.splitOn "-" val

            from :: Text
            from =
                singlePointVersion (List.head split)

            to :: Text
            to =
                singlePointVersion (List.last split)
        in
        [text|VersionRange $from $to|]

    else if "all" == val then
        "AllVersions"

    else if "TP" == val then
        "TechnologyPreview"

    else
        let
            single =
                singlePointVersion val
        in
        [text|VersionNumber $single|]



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
