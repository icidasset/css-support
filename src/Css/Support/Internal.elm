module Css.Support.Internal exposing (..)

import Css.Support.Data exposing (BrowserSupport, Supported(..), Version(..))


-- Supported


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



-- Versions


includesVersion : Version -> Version -> Bool
includesVersion target source =
    case source of
        AllVersions ->
            True

        TechnologyPreview ->
            False

        VersionNumber a ->
            case target of
                VersionNumber x ->
                    x >= a

                VersionRange x y ->
                    a >= x && a <= y

                _ ->
                    False

        VersionRange a b ->
            case target of
                VersionNumber x ->
                    x >= a && x <= b

                VersionRange x y ->
                    x >= a && y <= b

                _ ->
                    False
