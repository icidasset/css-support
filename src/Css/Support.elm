module Css.Support exposing (..)

{-| Functions that help you check the browser support for CSS properties.


# Compatibility

@docs compatible


# Targets

@docs Target, forTarget


# Compatibility + Targets

@docs caniuse

-}

import Css.Support.Data as Data exposing (Browser, BrowserSupport, Supported(..), Version(..))
import Css.Support.Internal exposing (..)
import Dict exposing (Dict)


-- Compatibility


{-| Find out which browsers fully, or partially, support a given CSS property.
Returns `Nothing` when it cannot find any associated data.


## Example

This is an example where we pick a single `BrowserSupport` from the result set.

    -- Filter for our results
    >>> internetExplorerOnly = .browser >> (==) Data.Ie

    -- Run `compatible` function
    >>> result =
    >>>     compatible "grid-row" { includePartialSupport = True }
    >>>         |> Maybe.map (List.filter internetExplorerOnly)
    >>>         |> Maybe.andThen (List.reverse >> List.head)
    >>>         |> Maybe.map .support

    -- 👩‍🔬
    >>> result
    Just PartiallySupportedWithPrefix

-}
compatible : String -> { includePartialSupport : Bool } -> Maybe (List BrowserSupport)
compatible cssProperty options =
    case Data.overlap cssProperty of
        [] ->
            Nothing

        overlap ->
            let
                filter =
                    if options.includePartialSupport then
                        atleastPartialSupport
                    else
                        atleastFullSupport
            in
                overlap
                    |> List.filter filter
                    |> Just



-- Targets


{-| A `Target` is the combination of a `Browser` and a `Version`.
-}
type alias Target =
    ( Browser, Version )


{-| Find out of the given browser-version combination supports the given CSS property.
Returns `NotSupported` when it cannot find any associated data.

    >>> forTarget "flex" (Data.Chrome, VersionNumber 60)
    Supported

    >>> forTarget "flex" (Data.Chrome, VersionNumber 22)
    SupportedWithPrefix

    >>> forTarget "flex" (Data.Chrome, VersionNumber 4)
    PartiallySupportedWithPrefix

    >>> forTarget "flex" (Data.Chrome, VersionNumber 1)
    NotSupported

-}
forTarget : String -> Target -> Supported
forTarget cssProperty ( browser, version ) =
    case Data.overlap cssProperty of
        [] ->
            NotSupported

        overlap ->
            overlap
                |> List.filter (.browser >> (==) browser)
                |> List.filter (.version >> includesVersion version)
                |> List.sortBy (.support >> supportedToComparable)
                |> List.head
                |> Maybe.map .support
                |> Maybe.withDefault NotSupported



-- Compatibility + Targets


{-| An alternative version of `forTarget` that takes:

  - A list of CSS properties instead of a single one
  - A list of `Target`s instead of a single one
  - Returns a `Dict` with `BrowserSupport`s

So in other words, this function will generate a compatibility table.


## Example

    >>> caniuse
    >>>   [ "align-items", "flex" ]
    >>>   [ (Data.Chrome, VersionNumber 5), (Data.Firefox, VersionNumber 30) ]
    Dict.fromList
        [ ( "align-items"
          , [ { browser = Data.Chrome
              , version = VersionNumber 5
              , note = Nothing
              , support = PartiallySupportedWithPrefix
              }
            , { browser = Data.Firefox
              , version = VersionNumber 30
              , note = Nothing
              , support = Supported
              }
            ]
          )
        , ( "flex"
          , [ { browser = Data.Chrome
              , version = VersionNumber 5
              , note = Nothing
              , support = PartiallySupportedWithPrefix
              }
            , { browser = Data.Firefox
              , version = VersionNumber 30
              , note = Nothing
              , support = Supported
              }
            ]
          )
        ]

-}
caniuse : List String -> List Target -> Dict String (List BrowserSupport)
caniuse cssProperties targets =
    let
        targetMap property target =
            { browser = Tuple.first target
            , version = Tuple.second target
            , note = Nothing
            , support = forTarget property target
            }

        propMap property =
            ( property
            , List.map (targetMap property) targets
            )
    in
        cssProperties
            |> List.map propMap
            |> Dict.fromList
