module Main
    ( main
    ) where

import Flow
import Protolude


-- Internal


import qualified Caniuse



-- 👨‍🚀


main :: IO ()
main = do
    set <- Caniuse.fetchData

    print set

    return ()
