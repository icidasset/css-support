module CssSupport exposing (..)

{-| Main.


# A

@docs a

-}


{-| -}
a : a -> a
a =
    identity



-- supportsProperty : String -> Bool -> List BrowserSupport
-- supportsProperty _ includePartialSupport =
--     [ { browser = Chrome
--       , version = 43
--       , support = Supported
--       }
--     , { browser = InternetExplorer
--       , version = 10
--       , support = NotSupported
--       }
--     ]
--
--
--
-- supportsTarget : String -> Target -> Supported
-- supportsTarget unprefixedCssProperty ( browser, version ) =
--     PartiallySupportedWithPrefix "with a different syntax"
--
--
--
-- caniuse : List String -> List Target -> Dict String (List BrowserSupport)
-- caniuse cssProps targets =
--     let
--         targetMap target =
--             { browser = Tuple.first target
--             , version = Tuple.second target
--             , support = supportsTarget prop target
--             }
--
--         propMap property =
--             ( prop
--             , List.map targetMap targets
--             )
--     in
--         cssProps
--             |> List.map propMap
--             |> Dict.fromList
