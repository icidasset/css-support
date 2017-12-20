module Main
    ( main
    ) where

import Flow
import Protolude


-- Internal


import qualified Caniuse
import qualified Elm
import qualified Mdn



-- 👨‍🚀


main :: IO ()
main = do
    caniuseData         <- Caniuse.fetchData
    cssProperties       <- Mdn.fetchData

    Elm.createModules caniuseData cssProperties
