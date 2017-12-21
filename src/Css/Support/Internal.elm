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


{-| Check if the `Version` named "target" (1st arg) is
included in the `Version` named "source" (2nd arg).


## Tests

    >>> includesVersion (VersionNumber 1.5) (AllVersions)
    True

    >>> includesVersion (VersionNumber 1.5) (TechnologyPreview)
    False

    >>> includesVersion (VersionNumber 1.5) (VersionNumber 1)
    True

    >>> includesVersion (VersionNumber 1.5) (VersionNumber 1.5)
    True

    >>> includesVersion (VersionNumber 1.5) (VersionNumber 3)
    False

    >>> includesVersion (VersionNumber 1.5) (VersionRange 1.5 2)
    True

    >>> includesVersion (VersionNumber 1.5) (VersionRange 1 2)
    True

    >>> includesVersion (VersionNumber 1.5) (VersionRange 2 3)
    False

    -- Part two

    >>> includesVersion (VersionRange 1 2) (VersionRange 0 3)
    True

    >>> includesVersion (VersionRange 1 2) (VersionRange 1 2)
    True

    >>> includesVersion (VersionRange 2 3) (VersionRange 1 2)
    False

    >>> includesVersion (VersionRange 2 3) (VersionRange 0 1)
    False

    >>> includesVersion (VersionRange 1 2) (VersionNumber 1)
    True

    >>> includesVersion (VersionRange 1 2) (VersionNumber 2)
    True

    >>> includesVersion (VersionRange 1 2) (VersionNumber 3)
    False

-}
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
