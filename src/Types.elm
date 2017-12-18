module Types exposing (..)


type Supported
    = Supported
    | SupportedWithPrefix
    | PartiallySupported String
    | PartiallySupportedWithPrefix String
    | NotSupported


type Browser
    = Chrome
    | Edge
    | Firefox
    | InternetExplorer
    | WebKit


type alias BrowserSupport =
    { browser : Browser
    , support : Supported
    , version : Float
    }


type alias Target =
    ( Browser, Float )



--


supportsProperty : String -> Bool -> List BrowserSupport
supportsProperty _ includePartialSupport =
    [ { browser = Chrome
      , version = 43
      , support = Supported
      }
    , { browser = InternetExplorer
      , version = 10
      , support = NotSupported
      }
    ]



--


supportsTarget : String -> Target -> Support
supportsTarget unprefixedCssProperty ( browser, version ) =
    PartiallySupportedWithPrefix "with a different syntax"



--


caniuse : List String -> List Target -> Dict String (List BrowserSupport)
caniuse =
    let
        targetMap target =
            { browser = Tuple.first target
            , version = Tuple.second target
            , support = supportsTarget prop target
            }

        propMap property =
            ( prop
            , List.map targetMap targets
            )
    in
        cssProps
            |> List.map propMap
            |> Dict.fromList
