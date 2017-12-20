module Css.Support.Internal exposing (..)

import Css.Support.Data exposing (BrowserSupport, Supported(..))


atleastPartialSupport : BrowserSupport -> Bool
atleastPartialSupport bs =
    case bs.support of
        NotSupported ->
            False

        _ ->
            True


atleastFullSupport : BrowserSupport -> Bool
atleastFullSupport bs =
    case bs.support of
        Supported ->
            True

        SupportedWithPrefix ->
            True

        _ ->
            False


supportedToComparable : Supported -> comparable
supportedToComparable sup =
    case sup of
        Supported ->
            1

        SupportedWithPrefix ->
            2

        PartiallySupported ->
            3

        PartiallySupportedWithPrefix ->
            4

        NotSupported ->
            5
