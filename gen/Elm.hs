module Elm where

import Data.HashMap.Strict (HashMap, (!))
import Flow
import NeatInterpolation
import Prelude (error)
import Protolude

import qualified Cases
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text


-- Internal


import Caniuse (Agent(..), DataSet(..), Entry(..), singlePointVersion, statisticSortFn)



-- 🍯


createModules :: DataSet -> [Text] -> IO ()
createModules set cssProperties = do
    writeFile
        "src/Css/Support/Data.elm"
        (renderModule "Css.Support.Data" set cssProperties)



-- DataSet -> Elm module


renderModule :: Text -> DataSet -> [Text] -> Text
renderModule name set cssPropsList =
    let
        agentsMap :: HashMap Text Agent
        agentsMap =
            (agents set)

        entriesList :: [(Text, Entry)]
        entriesList =
            (entries set)
                |> HashMap.toList
                |> List.sortOn fst

        -- Text

        browsers :: Text
        browsers =
            agentsMap
                |> HashMap.toList
                |> map (snd .> browser .> browserUnionValue)
                |> List.sort
                |> prepUnionValues
                |> Text.intercalate "\n"

        cssProps :: Text
        cssProps =
            cssPropsList
                |> map (\s -> Text.concat [ "\"", s, "\"" ])
                |> Text.intercalate ", "

        features :: Text
        features =
            entriesList
                |> map (renderEntry agentsMap)
                |> Text.intercalate "\n\n\n"

        featureFunctionNames :: Text
        featureFunctionNames =
            entriesList
                |> map (fst .> Cases.camelize)
                |> Text.intercalate ", "

        overlap :: Text
        overlap =
            cssPropsList
                |> map (findOverlap entriesList)
                |> filter Maybe.isJust
                |> map Maybe.fromJust
                |> map
                    (\(cssProp, featureFunctionName) ->
                        [text|
                        "$cssProp" ->
                            $featureFunctionName
                        |]
                    )
                |> Text.intercalate "\n"

        prefixes :: Text
        prefixes =
            agentsMap
                |> HashMap.toList
                |> map
                    (\(_, agent) ->
                        (,)
                            (browserUnionValue (browser agent))
                            (prefix agent)
                    )
                |> List.sortOn fst
                |> map
                    (\(browser, prefixText) ->
                        [text|
                        $browser ->
                            "$prefixText"
                        |]
                    )
                |> Text.intercalate "\n"
    in
    [text|
        module ${name} exposing (..)

        {-| All the [caniuse.com](https://caniuse.com/) data related to CSS reduced to a more concise data format (more info in the next paragraph), a list of all the standardized CSS properties from [MDN](https://developer.mozilla.org/en-US/) and the overlap between these two data sets.

        ## What exactly is in the `BrowserSupport` lists?

        __Scenario 1:__ The browser has support for the feature, it will list the first version that has partial support for the feature and also the version that has full support.

        __Scenario 2:__ The browser has no support for the feature, it will list the first version available from the caniuse data. If there are versions with other notes, it will list those as well.

        That's pretty much the gist of it.


        # Types

        @docs Browser, BrowserSupport, Supported, Version


        # CSS

        @docs standardCssProperties, prefixFor


        # Overlap

        @docs overlap


        # Features

        @docs $featureFunctionNames

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
        type Version
            = VersionNumber Float
            | VersionRange Float Float
            --
            | AllVersions
            | TechnologyPreview



        -- CSS


        {-| A list of all the standardized CSS properties.
        -}
        standardCssProperties : List String
        standardCssProperties =
            [ $cssProps ]


        {-| Get the css-property prefix associated with a given `Browser`.
        -}
        prefixFor : Browser -> String
        prefixFor browser =
            case browser of
                $prefixes



        -- Overlap


        {-| The overlap between the list of CSS properties (ie. `standardCssProperties`) and the features listed below. So in other words, you give this function a css property (that is part of the standard CSS spec) and then you'll get the browser support for it.
        -}
        overlap : String -> List BrowserSupport
        overlap cssProperty =
            case cssProperty of
                $overlap

                _ ->
                    []



        -- Features


        $features
    |]



-- Entries -> Elm functions


renderEntry :: HashMap Text Agent -> (Text, Entry) -> Text
renderEntry agents (key, entry) =
    let
        fnName :: Text
        fnName =
            Cases.camelize key

        statistics :: Text
        statistics =
            (stats entry)
                |> HashMap.toList
                |> List.sortOn fst
                |> map (renderStatGroup agents entry)
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


renderStatGroup :: HashMap Text Agent -> Entry -> (Text, HashMap Text Text) -> Text
renderStatGroup agents entry (key, statGroup) =
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
        |> map (renderStat entry browserValue)
        |> Text.intercalate "\n, "


renderStat :: Entry -> Text -> (Text, Text) -> Text
renderStat entry browserValue (key, stat) =
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



-- Overlap


findOverlap :: [(Text, Entry)] -> Text -> Maybe (Text, Text)
findOverlap entriesList cssProperty =
    entriesList
        |> List.map (insertOverlapScore cssProperty)
        |> List.filter (\(score, _) -> score > 0)
        |> List.sortOn (fst)
        |> lastMay
        |> map (snd .> Cases.camelize .> (,) cssProperty)


ignoredOverlap :: [Text]
ignoredOverlap =
    [ "background"
    , "background-color"
    , "background-position"
    , "background-repeat"
    , "border"
    , "bottom"
    , "color"
    , "cursor"
    , "display"
    , "font-size"
    , "font-weight"
    , "height"
    , "left"
    , "line-height"
    , "right"
    , "text-align"
    , "top"
    , "width"
    ]


insertOverlapScore :: Text -> (Text, Entry) -> (Int, Text)
insertOverlapScore cssProperty (key, entry) =
    (overlapScore cssProperty key entry, key)


overlapScore :: Text -> Text -> Entry -> Int
overlapScore prop key entry =
    List.foldl
        (\score (x, bool) ->
            if bool then score + x
            else score
        )
        (
            if List.elem prop ignoredOverlap then
                -1000000
            else
                0
        )
        [ (100      , key == prop                                                           )
        , (100      , key == Text.concat [ "css-", prop ]                                   )
        , (25       , Text.isInfixOf (Text.concat [ "`", prop, "`" ]) (description entry)   )
        , (5        , List.elem prop (keywords entry)                                       )
        ]



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

    else if Text.isPrefixOf "u d" val then
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
