module Main
    ( main
    ) where

import Flow
import Protolude


-- Internal


import qualified Caniuse
import qualified Elm



-- 👨‍🚀


main :: IO ()
main =
    Caniuse.fetchData >>= Elm.createModules
