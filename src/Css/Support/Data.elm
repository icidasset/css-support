module Css.Support.Data exposing (..)

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

@docs backgroundAttachment, backgroundImgOpts, backgroundPositionXY, backgroundRepeatRoundSpace, borderImage, borderRadius, calc, chUnit, cssAll, cssAnimation, cssAppearance, cssApplyRule, cssAtCounterStyle, cssBackdropFilter, cssBackgroundOffsets, cssBackgroundblendmode, cssBoxdecorationbreak, cssBoxshadow, cssCanvas, cssCaretColor, cssCaseInsensitive, cssClipPath, cssConicGradients, cssContainment, cssCounters, cssCrispEdges, cssCrossFade, cssDefaultPseudo, cssDescendantGtgt, cssDeviceadaptation, cssDirPseudo, cssDisplayContents, cssElementFunction, cssExclusions, cssFeaturequeries, cssFilterFunction, cssFilters, cssFirstLetter, cssFirstLine, cssFixed, cssFocusVisible, cssFocusWithin, cssFontRenderingControls, cssFontStretch, cssGencontent, cssGradients, cssGrid, cssHangingPunctuation, cssHas, cssHyphens, cssImageOrientation, cssImageSet, cssInOutOfRange, cssIndeterminatePseudo, cssInitialLetter, cssInitialValue, cssLetterSpacing, cssLineClamp, cssLogicalProps, cssMarkerPseudo, cssMasks, cssMatchesPseudo, cssMediaInteraction, cssMediaResolution, cssMediaqueries, cssMixblendmode, cssMotionPaths, cssNamespaces, cssNotSelList, cssNthChildOf, cssOpacity, cssOptionalPseudo, cssOverflowAnchor, cssOverscrollBehavior, cssPageBreak, cssPaintApi, cssPlaceholder, cssPlaceholderShown, cssReadOnlyWrite, cssRebeccapurple, cssReflections, cssRegions, cssRepeatingGradients, cssResize, cssRevertValue, cssRrggbbaa, cssScrollBehavior, cssScrollbar, cssSel2, cssSel3, cssSelection, cssShapes, cssSnappoints, cssSticky, cssTable, cssTextAlignLast, cssTextIndent, cssTextJustify, cssTextOrientation, cssTextshadow, cssTouchAction, cssTransitions, cssUnsetValue, cssVariables, cssWidowsOrphans, cssWritingMode, cssZoom, css3Attr, css3Boxsizing, css3Colors, css3Cursors, css3CursorsNewer, css3Tabsize, currentcolor, devicepixelratio, flexbox, flowRoot, fontFamilySystemUi, fontFeature, fontKerning, fontLoading, fontSizeAdjust, fontSmooth, fontUnicodeRange, fontVariantAlternates, fontface, getcomputedstyle, inlineBlock, intrinsicWidth, kerningPairsLigatures, minmaxwh, multibackgrounds, multicolumn, objectFit, outline, pointerEvents, prefersReducedMotion, rem, runIn, styleScoped, svgCss, textDecoration, textEmphasis, textOverflow, textSizeAdjust, textStroke, transforms2D, transforms3D, ttf, userSelectNone, variableFonts, viewportUnits, willChange, wordBreak, wordwrap

-}


-- Types


{-|-}
type Browser
    = AndroidBrowser
    | BaiduBrowser
    | BlackberryBrowser
    | Chrome
    | ChromeForAndroid
    | Edge
    | Firefox
    | FirefoxForAndroid
    | Ie
    | IeMobile
    | IosSafari
    | Opera
    | OperaMini
    | OperaMobile
    | QqBrowser
    | Safari
    | SamsungInternet
    | UcBrowserForAndroid


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
    [ "align-content", "align-items", "align-self", "all", "animation", "animation-delay", "animation-direction", "animation-duration", "animation-fill-mode", "animation-iteration-count", "animation-name", "animation-play-state", "animation-timing-function", "backface-visibility", "background", "background-attachment", "background-blend-mode", "background-clip", "background-color", "background-image", "background-origin", "background-position", "background-repeat", "background-size", "block-size", "border", "border-block-end", "border-block-end-color", "border-block-end-style", "border-block-end-width", "border-block-start", "border-block-start-color", "border-block-start-style", "border-block-start-width", "border-bottom", "border-bottom-color", "border-bottom-left-radius", "border-bottom-right-radius", "border-bottom-style", "border-bottom-width", "border-collapse", "border-color", "border-image", "border-image-outset", "border-image-repeat", "border-image-slice", "border-image-source", "border-image-width", "border-inline-end", "border-inline-end-color", "border-inline-end-style", "border-inline-end-width", "border-inline-start", "border-inline-start-color", "border-inline-start-style", "border-inline-start-width", "border-left", "border-left-color", "border-left-style", "border-left-width", "border-radius", "border-right", "border-right-color", "border-right-style", "border-right-width", "border-spacing", "border-style", "border-top", "border-top-color", "border-top-left-radius", "border-top-right-radius", "border-top-style", "border-top-width", "border-width", "bottom", "box-decoration-break", "box-shadow", "box-sizing", "break-after", "break-before", "break-inside", "caption-side", "caret-color", "clear", "clip", "clip-path", "color", "column-count", "column-fill", "column-gap", "column-rule", "column-rule-color", "column-rule-style", "column-rule-width", "column-span", "column-width", "columns", "content", "counter-increment", "counter-reset", "cursor", "direction", "display", "empty-cells", "filter", "flex", "flex-basis", "flex-direction", "flex-flow", "flex-grow", "flex-shrink", "flex-wrap", "float", "font", "font-family", "font-feature-settings", "font-kerning", "font-language-override", "font-size", "font-size-adjust", "font-stretch", "font-style", "font-synthesis", "font-variant", "font-variant-alternates", "font-variant-caps", "font-variant-east-asian", "font-variant-ligatures", "font-variant-numeric", "font-variant-position", "font-weight", "grid", "grid-area", "grid-auto-columns", "grid-auto-flow", "grid-auto-rows", "grid-column", "grid-column-end", "grid-column-gap", "grid-column-start", "grid-gap", "grid-row", "grid-row-end", "grid-row-gap", "grid-row-start", "grid-template", "grid-template-areas", "grid-template-columns", "grid-template-rows", "hanging-punctuation", "height", "hyphens", "image-orientation", "image-rendering", "image-resolution", "inline-size", "isolation", "justify-content", "left", "letter-spacing", "line-break", "line-height", "list-style", "list-style-image", "list-style-position", "list-style-type", "margin", "margin-block-end", "margin-block-start", "margin-bottom", "margin-inline-end", "margin-inline-start", "margin-left", "margin-right", "margin-top", "mask", "mask-clip", "mask-composite", "mask-image", "mask-mode", "mask-origin", "mask-position", "mask-repeat", "mask-size", "mask-type", "max-height", "max-width", "min-block-size", "min-height", "min-inline-size", "min-width", "mix-blend-mode", "object-fit", "object-position", "offset-block-end", "offset-block-start", "offset-inline-end", "offset-inline-start", "opacity", "order", "orphans", "outline", "outline-color", "outline-offset", "outline-style", "outline-width", "overflow", "overflow-wrap", "overflow-x", "overflow-y", "padding", "padding-block-end", "padding-block-start", "padding-bottom", "padding-inline-end", "padding-inline-start", "padding-left", "padding-right", "padding-top", "page-break-after", "page-break-before", "page-break-inside", "perspective", "perspective-origin", "place-content", "pointer-events", "position", "quotes", "resize", "right", "rotate", "ruby-align", "ruby-merge", "ruby-position", "scale", "scroll-behavior", "scroll-snap-coordinate", "scroll-snap-destination", "scroll-snap-type", "shape-image-threshold", "shape-margin", "shape-outside", "tab-size", "table-layout", "text-align", "text-align-last", "text-combine-upright", "text-decoration", "text-decoration-color", "text-decoration-line", "text-decoration-style", "text-emphasis", "text-emphasis-color", "text-emphasis-position", "text-emphasis-style", "text-indent", "text-justify", "text-orientation", "text-overflow", "text-rendering", "text-shadow", "text-transform", "text-underline-position", "top", "touch-action", "transform", "transform-box", "transform-origin", "transform-style", "transition", "transition-delay", "transition-duration", "transition-property", "transition-timing-function", "translate", "unicode-bidi", "vertical-align", "visibility", "white-space", "widows", "width", "will-change", "word-break", "word-spacing", "word-wrap", "writing-mode", "z-index" ]


{-| Get the css-property prefix associated with a given `Browser`.
-}
prefixFor : Browser -> String
prefixFor browser =
    case browser of
        AndroidBrowser ->
            "webkit"
        
        BaiduBrowser ->
            "webkit"
        
        BlackberryBrowser ->
            "webkit"
        
        Chrome ->
            "webkit"
        
        ChromeForAndroid ->
            "webkit"
        
        Edge ->
            "ms"
        
        Firefox ->
            "moz"
        
        FirefoxForAndroid ->
            "moz"
        
        Ie ->
            "ms"
        
        IeMobile ->
            "ms"
        
        IosSafari ->
            "webkit"
        
        Opera ->
            "webkit"
        
        OperaMini ->
            "o"
        
        OperaMobile ->
            "o"
        
        QqBrowser ->
            "webkit"
        
        Safari ->
            "webkit"
        
        SamsungInternet ->
            "webkit"
        
        UcBrowserForAndroid ->
            "webkit"



-- Overlap


{-| The overlap between the list of CSS properties (ie. `standardCssProperties`) and the features listed below. So in other words, you give this function a css property (that is part of the standard CSS spec) and then you'll get the browser support for it.
-}
overlap : String -> List BrowserSupport
overlap cssProperty =
    case cssProperty of
        "align-content" ->
            flexbox
        
        "align-items" ->
            flexbox
        
        "align-self" ->
            flexbox
        
        "all" ->
            cssAll
        
        "animation" ->
            cssAnimation
        
        "animation-delay" ->
            cssAnimation
        
        "animation-duration" ->
            cssAnimation
        
        "animation-name" ->
            cssAnimation
        
        "animation-timing-function" ->
            cssAnimation
        
        "backface-visibility" ->
            transforms3D
        
        "background-attachment" ->
            backgroundAttachment
        
        "block-size" ->
            cssLogicalProps
        
        "border-image" ->
            borderImage
        
        "border-image-outset" ->
            borderImage
        
        "border-image-repeat" ->
            borderImage
        
        "border-image-slice" ->
            borderImage
        
        "border-image-source" ->
            borderImage
        
        "border-image-width" ->
            borderImage
        
        "border-radius" ->
            borderRadius
        
        "border-top-left-radius" ->
            borderRadius
        
        "break-after" ->
            multicolumn
        
        "break-before" ->
            multicolumn
        
        "break-inside" ->
            multicolumn
        
        "caret-color" ->
            cssCaretColor
        
        "clip" ->
            cssMasks
        
        "clip-path" ->
            cssClipPath
        
        "column-count" ->
            multicolumn
        
        "column-fill" ->
            multicolumn
        
        "column-gap" ->
            multicolumn
        
        "column-rule" ->
            multicolumn
        
        "column-span" ->
            multicolumn
        
        "column-width" ->
            multicolumn
        
        "content" ->
            css3Attr
        
        "counter-increment" ->
            cssCounters
        
        "counter-reset" ->
            cssCounters
        
        "direction" ->
            cssAll
        
        "filter" ->
            cssFilterFunction
        
        "flex" ->
            flexbox
        
        "flex-basis" ->
            flexbox
        
        "flex-direction" ->
            flexbox
        
        "flex-flow" ->
            flexbox
        
        "flex-grow" ->
            flexbox
        
        "flex-wrap" ->
            flexbox
        
        "font-family" ->
            fontFamilySystemUi
        
        "font-feature-settings" ->
            fontVariantAlternates
        
        "font-kerning" ->
            fontKerning
        
        "font-size-adjust" ->
            fontSizeAdjust
        
        "font-stretch" ->
            cssFontStretch
        
        "font-variant" ->
            fontVariantAlternates
        
        "font-variant-alternates" ->
            fontVariantAlternates
        
        "font-variant-ligatures" ->
            fontFeature
        
        "grid" ->
            cssGrid
        
        "grid-column" ->
            cssGrid
        
        "grid-row" ->
            cssGrid
        
        "grid-template" ->
            cssGrid
        
        "hanging-punctuation" ->
            cssHangingPunctuation
        
        "hyphens" ->
            cssHyphens
        
        "image-orientation" ->
            cssImageOrientation
        
        "image-rendering" ->
            cssCrispEdges
        
        "inline-size" ->
            cssLogicalProps
        
        "justify-content" ->
            flexbox
        
        "letter-spacing" ->
            cssLetterSpacing
        
        "mask" ->
            cssMasks
        
        "mask-clip" ->
            cssMasks
        
        "mask-image" ->
            cssMasks
        
        "mask-mode" ->
            cssMasks
        
        "mask-type" ->
            cssMasks
        
        "max-height" ->
            minmaxwh
        
        "max-width" ->
            minmaxwh
        
        "min-height" ->
            minmaxwh
        
        "min-width" ->
            minmaxwh
        
        "mix-blend-mode" ->
            cssMixblendmode
        
        "object-fit" ->
            objectFit
        
        "opacity" ->
            cssOpacity
        
        "order" ->
            flexbox
        
        "outline" ->
            outline
        
        "outline-color" ->
            outline
        
        "outline-offset" ->
            outline
        
        "outline-style" ->
            outline
        
        "outline-width" ->
            outline
        
        "page-break-after" ->
            cssPageBreak
        
        "page-break-before" ->
            cssPageBreak
        
        "page-break-inside" ->
            cssPageBreak
        
        "perspective" ->
            transforms3D
        
        "pointer-events" ->
            pointerEvents
        
        "resize" ->
            cssResize
        
        "rotate" ->
            transforms2D
        
        "scale" ->
            transforms2D
        
        "scroll-behavior" ->
            cssScrollBehavior
        
        "scroll-snap-coordinate" ->
            cssSnappoints
        
        "scroll-snap-destination" ->
            cssSnappoints
        
        "scroll-snap-type" ->
            cssSnappoints
        
        "shape-image-threshold" ->
            cssShapes
        
        "shape-margin" ->
            cssShapes
        
        "shape-outside" ->
            cssShapes
        
        "tab-size" ->
            css3Tabsize
        
        "table-layout" ->
            cssTable
        
        "text-align-last" ->
            cssTextAlignLast
        
        "text-decoration" ->
            textDecoration
        
        "text-decoration-color" ->
            textDecoration
        
        "text-decoration-line" ->
            textDecoration
        
        "text-decoration-style" ->
            textDecoration
        
        "text-emphasis" ->
            textEmphasis
        
        "text-emphasis-color" ->
            textEmphasis
        
        "text-emphasis-position" ->
            textEmphasis
        
        "text-emphasis-style" ->
            textEmphasis
        
        "text-indent" ->
            cssTextIndent
        
        "text-justify" ->
            cssTextJustify
        
        "text-orientation" ->
            cssTextOrientation
        
        "text-overflow" ->
            textOverflow
        
        "touch-action" ->
            cssTouchAction
        
        "transform" ->
            transforms3D
        
        "transform-origin" ->
            transforms2D
        
        "transform-style" ->
            transforms3D
        
        "transition-delay" ->
            cssTransitions
        
        "transition-duration" ->
            cssTransitions
        
        "transition-property" ->
            cssTransitions
        
        "transition-timing-function" ->
            cssTransitions
        
        "translate" ->
            transforms2D
        
        "unicode-bidi" ->
            cssAll
        
        "will-change" ->
            willChange
        
        "word-break" ->
            wordBreak
        
        "word-wrap" ->
            wordwrap
        
        "writing-mode" ->
            cssWritingMode

        _ ->
            []



-- Features


{-| background-attachment
-}
backgroundAttachment : List BrowserSupport
backgroundAttachment =
    [ { browser = ChromeForAndroid
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to supporting `fixed` but not `local`
          """
      , support = PartiallySupported
      , version = VersionNumber 4.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = NotSupported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Does not support `fixed`, and due [to a bug](https://bugs.chromium.org/p/chromium/issues/detail?id=627037) only supports `local` if a `border-radius` is set on the element.
          """
      , support = PartiallySupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support refers to supporting `local` but not `fixed`
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to supporting `fixed` but not `local`
          """
      , support = PartiallySupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 25
      }
    
    , { browser = Ie
      , note = Just """
          Partial support refers to supporting `fixed` but not `local`
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to supporting `fixed` but not `local`
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to supporting `fixed` but not `local`
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 10.5
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to supporting `fixed` but not `local`
          """
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Most mobile devices have a delay in updating the background position after scrolling a page with `fixed` backgrounds.
          """
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Does not support `fixed`, and due [to a bug](https://bugs.chromium.org/p/chromium/issues/detail?id=627037) only supports `local` if a `border-radius` is set on the element.
          """
      , support = PartiallySupported
      , version = VersionNumber 5
      }
    ]



{-| background-img-opts
-}
backgroundImgOpts : List BrowserSupport
backgroundImgOpts =
    [ { browser = ChromeForAndroid
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Does not support `background-size` values in the `background` shorthand
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.2
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Does not support `background-size` values in the `background` shorthand
          """
      , support = PartiallySupported
      , version = VersionNumber 3
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Does not support `background-size` values in the `background` shorthand
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Edge
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.6
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Ie
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Does not support `background-size` values in the `background` shorthand
          """
      , support = PartiallySupported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = PartiallySupported
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial support in Opera Mini refers to not supporting background sizing or background attachments. However Opera Mini 7.5 supports background sizing (including cover and contain values).
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 10.0 10.1
      }
    
    , { browser = Opera
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 10.5
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Firefox, Chrome and Safari support the unofficial `-webkit-background-clip: text` (only with prefix)
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| background-position-x-y
-}
backgroundPositionXY : List BrowserSupport
backgroundPositionXY =
    [ { browser = ChromeForAndroid
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 49
      }
    
    , { browser = Ie
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          A workaround for the lack of support in Firefox 31 - Firefox 48 is to use [CSS variables](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_variables). See [this Stack Overflow answer](https://stackoverflow.com/a/29282573/94197) for an example.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| background-repeat-round-space
-}
backgroundRepeatRoundSpace : List BrowserSupport
backgroundRepeatRoundSpace =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 32
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 49
      }
    
    , { browser = Ie
      , note = Just """
          IE9 does not appear to render "background-repeat: round" correctly.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.5
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 19
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| border-image
-}
borderImage : List BrowserSupport
borderImage =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support refers to not supporting `border-image-repeat: space`
          """
      , support = PartiallySupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Note that both the `border-style` and `border-width` must be specified (not set to `none` or 0) for border-images to work.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support refers to not supporting `border-image-repeat: space`
          """
      , support = PartiallySupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support refers to not supporting `border-image-repeat: space`
          """
      , support = PartiallySupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 30
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to not supporting `border-image-repeat: space`
          """
      , support = PartiallySupported
      , version = VersionNumber 51
      }
    
    , { browser = Edge
      , note = Just """
          Has a bug where `border-image` incorrectly overrides `border-style`. See [test case](https://codepen.io/Savago/pen/yYrgyK), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=99922), [discussion](https://github.com/whatwg/compat/issues/17)
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Edge
      , note = Just """
          Note that both the `border-style` and `border-width` must be specified (not set to `none` or 0) for border-images to work.
          """
      , support = Supported
      , version = VersionNumber 14
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.5
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to not supporting `border-image-repeat: space`
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Firefox
      , note = Just """
          Note that both the `border-style` and `border-width` must be specified (not set to `none` or 0) for border-images to work.
          """
      , support = Supported
      , version = VersionNumber 50
      }
    
    , { browser = Ie
      , note = Just """
          Note that both the `border-style` and `border-width` must be specified (not set to `none` or 0) for border-images to work.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IeMobile
      , note = Just """
          Note that both the `border-style` and `border-width` must be specified (not set to `none` or 0) for border-images to work.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Has a bug where `border-image` incorrectly overrides `border-style`. See [test case](https://codepen.io/Savago/pen/yYrgyK), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=99922), [discussion](https://github.com/whatwg/compat/issues/17)
          """
      , support = Supported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 10.5
      }
    
    , { browser = Opera
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11
      }
    
    , { browser = Opera
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to not supporting `border-image-repeat: space`
          """
      , support = PartiallySupported
      , version = VersionNumber 38
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 6
      }
    
    , { browser = Safari
      , note = Just """
          Has a bug where `border-image` incorrectly overrides `border-style`. See [test case](https://codepen.io/Savago/pen/yYrgyK), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=99922), [discussion](https://github.com/whatwg/compat/issues/17)
          """
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support refers to not supporting `border-image-repeat: space`
          """
      , support = PartiallySupported
      , version = VersionNumber 5
      }
    ]



{-| border-radius
-}
borderRadius : List BrowserSupport
borderRadius =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.2
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Dotted and dashed rounded border corners are rendered as solid in Firefox. [see bug](https://bugzilla.mozilla.org/show_bug.cgi?id=382721)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Just """
          Dotted and dashed rounded border corners are rendered as solid in Firefox. [see bug](https://bugzilla.mozilla.org/show_bug.cgi?id=382721)
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3
      }
    
    , { browser = Firefox
      , note = Just """
          Dotted and dashed rounded border corners are rendered as solid in Firefox. [see bug](https://bugzilla.mozilla.org/show_bug.cgi?id=382721)
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 50
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.5
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = Safari
      , note = Just """
          Safari 6.1 and earlier did not apply `border-radius` correctly to image borders: https://stackoverflow.com/q/17202128
          """
      , support = Supported
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| calc
-}
calc : List BrowserSupport
calc =
    [ { browser = ChromeForAndroid
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support in Android Browser 4.4 refers to the browser lacking the ability to multiply and divide values.
          """
      , support = PartiallySupported
      , version = VersionNumber 4.4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 19
      }
    
    , { browser = Chrome
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 26
      }
    
    , { browser = Edge
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Ie
      , note = Just """
          Partial support in IE9 refers to the browser crashing when used as a `background-position` value.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Ie
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = SupportedWithPrefix
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 6
      }
    
    , { browser = Safari
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Support can be somewhat emulated in older versions of IE using the non-standard `expression()` syntax.
      
      Due to the way browsers handle [sub-pixel rounding](http://ejohn.org/blog/sub-pixel-problems-in-css/) differently, layouts using `calc()` expressions may have unexpected results.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| ch-unit
-}
chUnit : List BrowserSupport
chUnit =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 27
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          IE supports the `ch` unit, but unlike other browsers its width is that specifically of the "0" glyph, not its surrounding space. As a result, 3ch for example is shorter than the width of the string "000" in IE.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-all
-}
cssAll : List BrowserSupport
cssAll =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.43 4.44
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 27
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 24
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-animation
-}
cssAnimation : List BrowserSupport
cssAnimation =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support in Android browser refers to buggy behavior in different scenarios.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 43
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 5
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support in Android browser refers to buggy behavior in different scenarios.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 30
      }
    
    , { browser = Safari
      , note = Just """
          Does not support the `steps()`, `step-start` & `step-end` timing functions
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-appearance
-}
cssAppearance : List BrowserSupport
cssAppearance =
    [ { browser = ChromeForAndroid
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 35
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 11
      }
    
    , { browser = IosSafari
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          The appearance property is supported with the `none` value, but not `auto`. WebKit, Blink, and Gecko browsers also support additional vendor specific values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-apply-rule
-}
cssApplyRule : List BrowserSupport
cssApplyRule =
    [ { browser = ChromeForAndroid
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag under about:flags
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag under about:flags
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag under about:flags
          """
      , support = NotSupported
      , version = VersionNumber 51
      }
    
    , { browser = Edge
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 15
      }
    
    , { browser = Firefox
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag under about:flags
          """
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag under about:flags
          """
      , support = NotSupported
      , version = VersionNumber 38
      }
    
    , { browser = Safari
      , note = Just """
          See also support for [CSS Variables](#feat=css-variables)
          """
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag under about:flags
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    ]



{-| css-at-counter-style
-}
cssAtCounterStyle : List BrowserSupport
cssAtCounterStyle =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Partial support in Firefox refers to lacking support for [image symbols](https://bugzilla.mozilla.org/show_bug.cgi?id=1024179)
          """
      , support = PartiallySupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support in Firefox refers to lacking support for [image symbols](https://bugzilla.mozilla.org/show_bug.cgi?id=1024179)
          """
      , support = PartiallySupported
      , version = VersionNumber 33
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-backdrop-filter
-}
cssBackdropFilter : List BrowserSupport
cssBackdropFilter =
    [ { browser = ChromeForAndroid
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 47
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 17
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 34
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    ]



{-| css-background-offsets
-}
cssBackgroundOffsets : List BrowserSupport
cssBackgroundOffsets =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 25
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 13
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.5
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-backgroundblendmode
-}
cssBackgroundblendmode : List BrowserSupport
cssBackgroundblendmode =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 35
      }
    
    , { browser = Chrome
      , note = Just """
          Chrome 46 has some [serious bugs](https://code.google.com/p/chromium/issues/detail?id=543583) with multiply, difference, and exclusion blend modes
          """
      , support = PartiallySupported
      , version = VersionNumber 46
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 47
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 30
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial in Safari refers to not supporting the `hue`, `saturation`, `color`, and `luminosity` blend modes.
          """
      , support = PartiallySupported
      , version = VersionNumber 8
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Chrome 46 has some [serious bugs](https://code.google.com/p/chromium/issues/detail?id=543583) with multiply, difference, and exclusion blend modes
          """
      , support = PartiallySupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 22
      }
    
    , { browser = Opera
      , note = Just """
          Chrome 46 has some [serious bugs](https://code.google.com/p/chromium/issues/detail?id=543583) with multiply, difference, and exclusion blend modes
          """
      , support = PartiallySupported
      , version = VersionNumber 33
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 34
      }
    
    , { browser = Safari
      , note = Just """
          Partial in Safari refers to not supporting the `hue`, `saturation`, `color`, and `luminosity` blend modes.
          """
      , support = PartiallySupported
      , version = VersionNumber 7.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-boxdecorationbreak
-}
cssBoxdecorationbreak : List BrowserSupport
cssBoxdecorationbreak =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 22
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 32
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support refers to working for inline elements but not across column or page breaks.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-boxshadow
-}
cssBoxshadow : List BrowserSupport
cssBoxshadow =
    [ { browser = ChromeForAndroid
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support in Safari, iOS Safari and Android Browser refers to missing "inset", blur radius value, and multiple shadow support.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Edge
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.5
      }
    
    , { browser = Firefox
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Ie
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support in Safari, iOS Safari and Android Browser refers to missing "inset", blur radius value, and multiple shadow support.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = SupportedWithPrefix
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Opera
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 10.5
      }
    
    , { browser = Safari
      , note = Just """
          Partial support in Safari, iOS Safari and Android Browser refers to missing "inset", blur radius value, and multiple shadow support.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 5
      }
    
    , { browser = Safari
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 5.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Can be partially emulated in older IE versions using the non-standard "shadow" filter.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-canvas
-}
cssCanvas : List BrowserSupport
cssCanvas =
    [ { browser = ChromeForAndroid
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 48
      }
    
    , { browser = Edge
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 52
      }
    
    , { browser = Ie
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 35
      }
    
    , { browser = Safari
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          A similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    ]



{-| css-caret-color
-}
cssCaretColor : List BrowserSupport
cssCaretColor =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 53
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 44
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-case-insensitive
-}
cssCaseInsensitive : List BrowserSupport
cssCaseInsensitive =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 49
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 47
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-clip-path
-}
cssClipPath : List BrowserSupport
cssClipPath =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Support refers to the `clip-path` CSS property on HTML elements specifically. Support for `clip-path` in SVG is supported in all browsers with [basic SVG](#feat=svg) support.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Support refers to the `clip-path` CSS property on HTML elements specifically. Support for `clip-path` in SVG is supported in all browsers with [basic SVG](#feat=svg) support.
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 24
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupported
      , version = VersionNumber 55
      }
    
    , { browser = Edge
      , note = Just """
          Support refers to the `clip-path` CSS property on HTML elements specifically. Support for `clip-path` in SVG is supported in all browsers with [basic SVG](#feat=svg) support.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to only supporting the `url()` syntax.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.5
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 47
      }
    
    , { browser = Firefox
      , note = Just """
          Support refers to the `clip-path` CSS property on HTML elements specifically. Support for `clip-path` in SVG is supported in all browsers with [basic SVG](#feat=svg) support.
          """
      , support = Supported
      , version = VersionNumber 54
      }
    
    , { browser = Ie
      , note = Just """
          Support refers to the `clip-path` CSS property on HTML elements specifically. Support for `clip-path` in SVG is supported in all browsers with [basic SVG](#feat=svg) support.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Support refers to the `clip-path` CSS property on HTML elements specifically. Support for `clip-path` in SVG is supported in all browsers with [basic SVG](#feat=svg) support.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Support refers to the `clip-path` CSS property on HTML elements specifically. Support for `clip-path` in SVG is supported in all browsers with [basic SVG](#feat=svg) support.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupported
      , version = VersionNumber 42
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support refers to supporting shapes and the `url(#foo)` syntax for inline SVG, but not shapes in external SVGs.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-conic-gradients
-}
cssConicGradients : List BrowserSupport
cssConicGradients =
    [ { browser = ChromeForAndroid
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 59
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 46
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-containment
-}
cssContainment : List BrowserSupport
cssContainment =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Partially supported in Firefox by enabling "layout.css.contain.enabled" in about:config
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled via the "Experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 51
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 52
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partially supported in Firefox by enabling "layout.css.contain.enabled" in about:config
          """
      , support = NotSupported
      , version = VersionNumber 41
      }
    
    , { browser = Firefox
      , note = Just """
          Partially supported in Firefox by enabling "layout.css.contain.enabled" in about:config
          """
      , support = PartiallySupported
      , version = VersionNumber 52
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Enabled via the "Experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 38
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 40
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 6.2
      }
    ]



{-| css-counters
-}
cssCounters : List BrowserSupport
cssCounters =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-crisp-edges
-}
cssCrispEdges : List BrowserSupport
cssCrispEdges =
    [ { browser = ChromeForAndroid
      , note = Just """
          Supports the `pixelated` value, but not `crisp-edges`.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Supports the `crisp-edges` value, but not `pixelated`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Note that prefixes apply to the value (e.g. `-moz-crisp-edges`), not the `image-rendering` property.
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Note that prefixes apply to the value (e.g. `-moz-crisp-edges`), not the `image-rendering` property.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Supports the `pixelated` value, but not `crisp-edges`.
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Supports the `pixelated` value, but not `crisp-edges`.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Supports the `pixelated` value, but not `crisp-edges`.
          """
      , support = Supported
      , version = VersionNumber 41
      }
    
    , { browser = Edge
      , note = Just """
          Note that prefixes apply to the value (e.g. `-moz-crisp-edges`), not the `image-rendering` property.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Supports the `crisp-edges` value, but not `pixelated`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.6
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Note that prefixes apply to the value (e.g. `-moz-crisp-edges`), not the `image-rendering` property.
          """
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Note that prefixes apply to the value (e.g. `-moz-crisp-edges`), not the `image-rendering` property.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Supports the `crisp-edges` value, but not `pixelated`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = OperaMobile
      , note = Just """
          Supports the `pixelated` value, but not `crisp-edges`.
          """
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Supports the `crisp-edges` value, but not `pixelated`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 11.6
      }
    
    , { browser = Opera
      , note = Just """
          Note that prefixes apply to the value (e.g. `-moz-crisp-edges`), not the `image-rendering` property.
          """
      , support = NotSupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Supports the `pixelated` value, but not `crisp-edges`.
          """
      , support = Supported
      , version = VersionNumber 28
      }
    
    , { browser = Safari
      , note = Just """
          Supported using the non-standard value `-webkit-optimize-contrast`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 6
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Just """
          Note that prefixes apply to the value (e.g. `-moz-crisp-edges`), not the `image-rendering` property.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Supports the `pixelated` value, but not `crisp-edges`.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-cross-fade
-}
cssCrossFade : List BrowserSupport
cssCrossFade =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 17
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-default-pseudo
-}
cssDefaultPseudo : List BrowserSupport
cssDefaultPseudo =
    [ { browser = ChromeForAndroid
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Does not match `<input type="checkbox" checked>` or `<input type="radio" checked>`
          """
      , support = PartiallySupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Does not match `<input type="checkbox" checked>` or `<input type="radio" checked>`
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Does not match `<input type="checkbox" checked>` or `<input type="radio" checked>`
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Does not match `<input type="checkbox" checked>` or `<input type="radio" checked>`
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Chrome
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 51
      }
    
    , { browser = Edge
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Ie
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Does not match `<input type="checkbox" checked>` or `<input type="radio" checked>`
          """
      , support = PartiallySupported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 10.3
      }
    
    , { browser = OperaMini
      , note = Just """
          Does not match the default submit button of a form
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Just """
          Does not match the default submit button of a form
          """
      , support = PartiallySupported
      , version = VersionNumber 12.1
      }
    
    , { browser = OperaMobile
      , note = Just """
          Does not match `<input type="checkbox" checked>` or `<input type="radio" checked>`
          """
      , support = PartiallySupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Does not match the default submit button of a form
          """
      , support = PartiallySupported
      , version = VersionNumber 11.6
      }
    
    , { browser = Opera
      , note = Just """
          Does not match `<input type="checkbox" checked>` or `<input type="radio" checked>`
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 38
      }
    
    , { browser = Safari
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Does not match `<input type="checkbox" checked>` or `<input type="radio" checked>`
          """
      , support = PartiallySupported
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Does not match `<input type="checkbox" checked>` or `<input type="radio" checked>`
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Whether `<option selected>` matches `:default` (per the spec) was not tested since `<select>`s and `<option>`s are generally not styleable, which makes it hard to formulate a test for this.
          """
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-descendant-gtgt
-}
cssDescendantGtgt : List BrowserSupport
cssDescendantGtgt =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 65
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-deviceadaptation
-}
cssDeviceadaptation : List BrowserSupport
cssDeviceadaptation =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 29
      }
    
    , { browser = Edge
      , note = Just """
          IE only supports the 'width' and 'height' properties.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          IE only supports the 'width' and 'height' properties.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          IE only supports the 'width' and 'height' properties.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Opera Mobile and Opera Mini only support the 'orientation' property.
          """
      , support = PartiallySupportedWithPrefix
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Opera Mobile and Opera Mini only support the 'orientation' property.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 40
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-dir-pseudo
-}
cssDirPseudo : List BrowserSupport
cssDirPseudo =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 17
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 49
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-display-contents
-}
cssDisplayContents : List BrowserSupport
cssDisplayContents =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled in Chrome through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 58
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 65
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 52
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-element-function
-}
cssElementFunction : List BrowserSupport
cssElementFunction =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          In Firefox < 4, usage limited to the background and background-image CSS properties
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-exclusions
-}
cssExclusions : List BrowserSupport
cssExclusions =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-featurequeries
-}
cssFeaturequeries : List BrowserSupport
cssFeaturequeries =
    [ { browser = ChromeForAndroid
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 28
      }
    
    , { browser = Edge
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 22
      }
    
    , { browser = Ie
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Safari
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Just """
          See also the [CSS.supports() DOM API](#feat=css-supports-api)
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-filter-function
-}
cssFilterFunction : List BrowserSupport
cssFilterFunction =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-filters
-}
cssFilters : List BrowserSupport
cssFilters =
    [ { browser = ChromeForAndroid
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 18
      }
    
    , { browser = Chrome
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = Supported
      , version = VersionNumber 53
      }
    
    , { browser = Edge
      , note = Just """
          Partial support refers to supporting filter functions, but not the `url` function.
          """
      , support = PartiallySupported
      , version = VersionNumber 13
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support in Firefox before version 34 [only implemented the url() function of the filter property](https://developer.mozilla.org/en-US/docs/Web/CSS/filter#Browser_compatibility)
          """
      , support = PartiallySupported
      , version = VersionNumber 3.6
      }
    
    , { browser = Firefox
      , note = Just """
          Supported in Firefox under the `layout.css.filters.enabled` flag.
          """
      , support = PartiallySupported
      , version = VersionNumber 34
      }
    
    , { browser = Firefox
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = Supported
      , version = VersionNumber 35
      }
    
    , { browser = Ie
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = Supported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = Supported
      , version = VersionNumber 40
      }
    
    , { browser = Safari
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 6
      }
    
    , { browser = Safari
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Note that this property is significantly different from and incompatible with Microsoft's [older "filter" property](http://msdn.microsoft.com/en-us/library/ie/ms530752%28v=vs.85%29.aspx).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-first-letter
-}
cssFirstLetter : List BrowserSupport
cssFirstLetter =
    [ { browser = ChromeForAndroid
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Excludes punctuation immediately after the first letter from the match. (The spec says it should be included in the match.)
          """
      , support = PartiallySupported
      , version = VersionNumber 2.3
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 3
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Excludes punctuation immediately after the first letter from the match. (The spec says it should be included in the match.)
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    
    , { browser = Chrome
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Edge
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Acts like the first character is always a letter even when it's not. For example, given "!,X;", "!," is matched instead of the entire string.
          """
      , support = PartiallySupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Just """
          Excludes punctuation immediately after the first letter from the match. (The spec says it should be included in the match.)
          """
      , support = PartiallySupported
      , version = VersionNumber 3
      }
    
    , { browser = Firefox
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 3.5
      }
    
    , { browser = Ie
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 6
      }
    
    , { browser = Ie
      , note = Just """
          Only recognizes the deprecated :first-letter pseudo-class, not the ::first-letter pseudo-element.
          """
      , support = PartiallySupported
      , version = VersionNumber 8
      }
    
    , { browser = Ie
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = OperaMini
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Acts like the first character is always a letter even when it's not. For example, given "!,X;", "!," is matched instead of the entire string.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Opera
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Acts like the first character is always a letter even when it's not. For example, given "!,X;", "!," is matched instead of the entire string.
          """
      , support = PartiallySupported
      , version = VersionRange 10.0 10.1
      }
    
    , { browser = Opera
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 11.6
      }
    
    , { browser = Safari
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Excludes punctuation immediately after the first letter from the match. (The spec says it should be included in the match.)
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = Safari
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    
    , { browser = Safari
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 5.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          The spec says that both letters of digraphs which are always capitalized together (such as "IJ" in Dutch) should be matched by ::first-letter, but no browser has ever implemented this.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-first-line
-}
cssFirstLine : List BrowserSupport
cssFirstLine =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          IE8 only supports the single-colon CSS 2.1 syntax (i.e. `:first-line`). It does not support the double-colon CSS3 syntax (i.e. `::first-line`).
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-fixed
-}
cssFixed : List BrowserSupport
cssFixed =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Only works in Android 2.1 thru 2.3 by using the following meta tag: <meta name="viewport" content="width=device-width, user-scalable=no">.
          """
      , support = PartiallySupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Android 4.0-4.3 [ignore transforms and margin:auto on position:fixed elements](https://codepen.io/mattiacci/pen/mPRKZY).
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 6
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support in older iOS Safari refers to [buggy behavior](http://remysharp.com/2012/05/24/issues-with-position-fixed-scrolling-on-ios/).
          """
      , support = PartiallySupported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          in Safari 9.1, having a `position:fixed`-element inside an animated element, [may cause the `position:fixed`-element to not appear](https://jsbin.com/fuxipax).
          """
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-focus-visible
-}
cssFocusVisible : List BrowserSupport
cssFocusVisible =
    [ { browser = ChromeForAndroid
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          As `:-moz-focusring`
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          As `:-moz-focusring`
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Ie
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Previously drafted as `:focus-ring`
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-focus-within
-}
cssFocusWithin : List BrowserSupport
cssFocusWithin =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 59
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 60
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 52
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Can be enabled via the "Experimental Web Platform Features" flag
          """
      , support = NotSupported
      , version = VersionNumber 46
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 47
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-font-rendering-controls
-}
cssFontRenderingControls : List BrowserSupport
cssFontRenderingControls =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Can be enabled via the "Experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Can be enabled via the "Experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 49
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 60
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Can be enabled in Firefox through the `layout.css.font-display.enabled` flag at about:config
          """
      , support = NotSupported
      , version = VersionNumber 46
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 58
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Can be enabled via the "Experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Can be enabled via the "Experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 36
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 47
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Can be enabled via the "Experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    ]



{-| css-font-stretch
-}
cssFontStretch : List BrowserSupport
cssFontStretch =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 48
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 35
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-gencontent
-}
cssGencontent : List BrowserSupport
cssGencontent =
    [ { browser = ChromeForAndroid
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          IE8 only supports the single-colon CSS 2.1 syntax (i.e. :pseudo-class). It does not support the double-colon CSS3 syntax (i.e. ::pseudo-element).
          """
      , support = PartiallySupported
      , version = VersionNumber 8
      }
    
    , { browser = Ie
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          For content to appear in pseudo-elements, the `content` property must be set (but may be an empty string).
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-gradients
-}
cssGradients : List BrowserSupport
cssGradients =
    [ { browser = ChromeForAndroid
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 26
      }
    
    , { browser = Edge
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.6
      }
    
    , { browser = Firefox
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Ie
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support in Opera 11.10 and 11.50 also refers to only having support for linear gradients.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.1
      }
    
    , { browser = OperaMobile
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = OperaMobile
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Just """
          Partial support in Opera 11.10 and 11.50 also refers to only having support for linear gradients.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.1
      }
    
    , { browser = Opera
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 11.6
      }
    
    , { browser = Opera
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Safari
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Safari
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Syntax used by browsers with prefixed support may be incompatible with that for proper support.
      
      Support can be somewhat emulated in older IE versions using the non-standard "gradient" filter. 
      
      Firefox 10+, Opera 11.6+, Chrome 26+ and IE10+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-grid
-}
cssGrid : List BrowserSupport
cssGrid =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Enabled in Chrome through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionRange 4.2 4.3
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 25
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled in Chrome through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 29
      }
    
    , { browser = Chrome
      , note = Just """
          There are some bugs with overflow ([1356820](https://bugzilla.mozilla.org/show_bug.cgi?id=1356820), [1348857](https://bugzilla.mozilla.org/show_bug.cgi?id=1348857), [1350925](https://bugzilla.mozilla.org/show_bug.cgi?id=1350925))
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 58
      }
    
    , { browser = Edge
      , note = Just """
          Partial support in IE refers to supporting an [older version](https://www.w3.org/TR/2011/WD-css3-grid-layout-20110407/) of the specification.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 19
      }
    
    , { browser = Firefox
      , note = Just """
          Enabled in Firefox through the `layout.css.grid.enabled ` flag
          """
      , support = NotSupported
      , version = VersionNumber 40
      }
    
    , { browser = Firefox
      , note = Just """
          There are some bugs with overflow ([1356820](https://bugzilla.mozilla.org/show_bug.cgi?id=1356820), [1348857](https://bugzilla.mozilla.org/show_bug.cgi?id=1348857), [1350925](https://bugzilla.mozilla.org/show_bug.cgi?id=1350925))
          """
      , support = Supported
      , version = VersionNumber 52
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 54
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Ie
      , note = Just """
          Partial support in IE refers to supporting an [older version](https://www.w3.org/TR/2011/WD-css3-grid-layout-20110407/) of the specification.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Partial support in IE refers to supporting an [older version](https://www.w3.org/TR/2011/WD-css3-grid-layout-20110407/) of the specification.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Enabled in Chrome through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 28
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 44
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 6
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 6.2
      }
    ]



{-| css-hanging-punctuation
-}
cssHangingPunctuation : List BrowserSupport
cssHangingPunctuation =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 15
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-has
-}
cssHas : List BrowserSupport
cssHas =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-hyphens
-}
cssHyphens : List BrowserSupport
cssHyphens =
    [ { browser = ChromeForAndroid
      , note = Just """
          Only supported on Android & Mac platforms (and only the "auto" value) for now. [See commit](https://crrev.com/ed7e106e0e48b3afb160a5bdbb37649e307d2b05) & related [bug](https://bugs.chromium.org/p/chromium/issues/detail?id=652964).
          """
      , support = PartiallySupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = PartiallySupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Only supported on Android & Mac platforms (and only the "auto" value) for now. [See commit](https://crrev.com/ed7e106e0e48b3afb160a5bdbb37649e307d2b05) & related [bug](https://bugs.chromium.org/p/chromium/issues/detail?id=652964).
          """
      , support = PartiallySupported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Only supported on Android & Mac platforms (and only the "auto" value) for now. [See commit](https://crrev.com/ed7e106e0e48b3afb160a5bdbb37649e307d2b05) & related [bug](https://bugs.chromium.org/p/chromium/issues/detail?id=652964).
          """
      , support = PartiallySupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Only supported on Android & Mac platforms (and only the "auto" value) for now. [See commit](https://crrev.com/ed7e106e0e48b3afb160a5bdbb37649e307d2b05) & related [bug](https://bugs.chromium.org/p/chromium/issues/detail?id=652964).
          """
      , support = PartiallySupported
      , version = VersionNumber 55
      }
    
    , { browser = Edge
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 6
      }
    
    , { browser = Firefox
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = Supported
      , version = VersionNumber 43
      }
    
    , { browser = Ie
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = SupportedWithPrefix
      , version = VersionRange 4.2 4.3
      }
    
    , { browser = OperaMini
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Only supported on Android & Mac platforms (and only the "auto" value) for now. [See commit](https://crrev.com/ed7e106e0e48b3afb160a5bdbb37649e307d2b05) & related [bug](https://bugs.chromium.org/p/chromium/issues/detail?id=652964).
          """
      , support = PartiallySupported
      , version = VersionNumber 42
      }
    
    , { browser = Safari
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 5.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Only supported on Android & Mac platforms (and only the "auto" value) for now. [See commit](https://crrev.com/ed7e106e0e48b3afb160a5bdbb37649e307d2b05) & related [bug](https://bugs.chromium.org/p/chromium/issues/detail?id=652964).
          """
      , support = PartiallySupported
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Chrome < 55 and Android 4.0 Browser support "-webkit-hyphens: none", but not the "auto" property. It is [advisable to set the @lang attribute](http://blog.adrianroselli.com/2015/01/on-use-of-lang-attribute.html) on the HTML element to enable hyphenation support and improve accessibility.
          """
      , support = Supported
      , version = VersionNumber 6.2
      }
    ]



{-| css-image-orientation
-}
cssImageOrientation : List BrowserSupport
cssImageOrientation =
    [ { browser = ChromeForAndroid
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = Supported
      , version = VersionNumber 26
      }
    
    , { browser = Ie
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support in iOS refers to the browser using EXIF data by default, though it does not actually support the property.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Opening the image in a new tab in Chrome results in the image shown in the orientation according to the EXIF data.
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-image-set
-}
cssImageSet : List BrowserSupport
cssImageSet =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 21
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Safari's implementation does not completely match the spec, in that only URLs are accepted for the image value and only 'x' is accepted as a resolution. See https://bugs.webkit.org/show_bug.cgi?id=160934.
          """
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 6
      }
    
    , { browser = Safari
      , note = Just """
          Safari's implementation does not completely match the spec, in that only URLs are accepted for the image value and only 'x' is accepted as a resolution. See https://bugs.webkit.org/show_bug.cgi?id=160934.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-in-out-of-range
-}
cssInOutOfRange : List BrowserSupport
cssInOutOfRange =
    [ { browser = ChromeForAndroid
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          `:in-range` also incorrectly matches temporal and `number` inputs which don't have `min` or `max` attributes. See [Edge bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7200501/), [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=603268), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=156558).
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          `:in-range` also incorrectly matches temporal and `number` inputs which don't have `min` or `max` attributes. See [Edge bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7200501/), [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=603268), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=156558).
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Chrome
      , note = Just """
          `:in-range` also incorrectly matches temporal and `number` inputs which don't have `min` or `max` attributes. See [Edge bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7200501/), [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=603268), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=156558).
          """
      , support = PartiallySupported
      , version = VersionNumber 52
      }
    
    , { browser = Chrome
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 53
      }
    
    , { browser = Edge
      , note = Just """
          `:in-range` also incorrectly matches temporal and `number` inputs which don't have `min` or `max` attributes. See [Edge bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7200501/), [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=603268), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=156558).
          """
      , support = PartiallySupported
      , version = VersionNumber 13
      }
    
    , { browser = Firefox
      , note = Just """
          `:in-range` and `:out-of-range` incorrectly match inputs which are disabled or readonly. See [Edge bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7190958/), [Mozilla bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1264157), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=156530), [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=602568).
          """
      , support = PartiallySupported
      , version = VersionNumber 29
      }
    
    , { browser = Firefox
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 50
      }
    
    , { browser = Ie
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 10.3
      }
    
    , { browser = OperaMini
      , note = Just """
          Opera Mini correctly applies style on initial load, but does not correctly update when value is changed.
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          `:in-range` also incorrectly matches temporal and `number` inputs which don't have `min` or `max` attributes. See [Edge bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7200501/), [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=603268), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=156558).
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          `:in-range` also incorrectly matches temporal and `number` inputs which don't have `min` or `max` attributes. See [Edge bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7200501/), [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=603268), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=156558).
          """
      , support = PartiallySupported
      , version = VersionRange 10.0 10.1
      }
    
    , { browser = Opera
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          `:in-range` also incorrectly matches temporal and `number` inputs which don't have `min` or `max` attributes. See [Edge bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7200501/), [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=603268), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=156558).
          """
      , support = PartiallySupported
      , version = VersionNumber 39
      }
    
    , { browser = Opera
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 40
      }
    
    , { browser = Safari
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          `:in-range` also incorrectly matches temporal and `number` inputs which don't have `min` or `max` attributes. See [Edge bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7200501/), [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=603268), [WebKit bug](https://bugs.webkit.org/show_bug.cgi?id=156558).
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Note that `<input type="range">` can never match `:out-of-range` because the user cannot input such a value, and if the initial value is outside the range, the browser immediately clamps it to the minimum or maximum (as appropriate) bound of the range.
          """
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-indeterminate-pseudo
-}
cssIndeterminatePseudo : List BrowserSupport
cssIndeterminatePseudo =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 4.4
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionRange 4.43 4.44
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 39
      }
    
    , { browser = Edge
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 6
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 51
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Ie
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.3
      }
    
    , { browser = OperaMini
      , note = Just """
          Doesn't match indeterminate `<progress>` bars
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Just """
          Doesn't match indeterminate `<progress>` bars
          """
      , support = PartiallySupported
      , version = VersionNumber 12.1
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Doesn't match indeterminate `<progress>` bars
          """
      , support = PartiallySupported
      , version = VersionNumber 11.6
      }
    
    , { browser = Opera
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 26
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 6
      }
    
    , { browser = Safari
      , note = Just """
          Doesn't match radio buttons whose radio button group lacks a checked radio button
          """
      , support = PartiallySupported
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-initial-letter
-}
cssInitialLetter : List BrowserSupport
cssInitialLetter =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Safari implementation is incomplete. Does not allow applying web fonts to the initial letter.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          Safari implementation is incomplete. Does not allow applying web fonts to the initial letter.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-initial-value
-}
cssInitialValue : List BrowserSupport
cssInitialValue =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.3
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 19
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-letter-spacing
-}
cssLetterSpacing : List BrowserSupport
cssLetterSpacing =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Truncates or rounds fractional portions of values
          """
      , support = PartiallySupported
      , version = VersionNumber 2.3
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Truncates or rounds fractional portions of values
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Truncates or rounds fractional portions of values
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 30
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          Truncates or rounds fractional portions of values
          """
      , support = PartiallySupported
      , version = VersionNumber 6
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Truncates or rounds fractional portions of values
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Truncates or rounds fractional portions of values
          """
      , support = PartiallySupported
      , version = VersionRange 10.0 10.1
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 17
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Truncates or rounds fractional portions of values
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-line-clamp
-}
cssLineClamp : List BrowserSupport
cssLineClamp =
    [ { browser = ChromeForAndroid
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 2.3
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 14
      }
    
    , { browser = Edge
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = OperaMini
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Just """
          As there is no specification and the property is dependent on an outdated implementation of flexbox (hence `display: -webkit-box`) it is unlikely that other browsers will support the property as-is, although an alternative solution may at some point replace it.
      
      Older (presto-based) versions of the Opera browser have also supported the same effect using the proprietary `-o-ellipsis-lastline;` value for `text-overflow`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-logical-props
-}
cssLogicalProps : List BrowserSupport
cssLogicalProps =
    [ { browser = ChromeForAndroid
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Only supports the *-start, and *-end values for `margin`, `border` and `padding`, not the inline/block type values as defined in the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Only supports the *-start, and *-end values for `margin`, `border` and `padding`, not the inline/block type values as defined in the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 41
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Like #1 but also supports `*-before` and `*-end` for `*-block-start` and `*-block-end` properties as well as `start` and `end` values for `text-align`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-marker-pseudo
-}
cssMarkerPseudo : List BrowserSupport
cssMarkerPseudo =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          Safari supports [the definition in the CSS Pseudo-Elements Module Level 4](https://www.w3.org/TR/css-pseudo-4/#marker-pseudo) instead of one in the List module due to the flux definition in it.
          """
      , support = Supported
      , version = VersionNumber 11.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-masks
-}
cssMasks : List BrowserSupport
cssMasks =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support in Firefox refers to only support for inline SVG mask elements i.e. mask: url(#foo).
          """
      , support = PartiallySupported
      , version = VersionNumber 3.5
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 53
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support in WebKit/Blink browsers refers to supporting the mask-image and mask-box-image properties, but lacking support for other parts of the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-matches-pseudo
-}
cssMatchesPseudo : List BrowserSupport
cssMatchesPseudo =
    [ { browser = ChromeForAndroid
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Only supports the `:-moz-any()` pseudo-class.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Edge
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Just """
          Only supports the `:-moz-any()` pseudo-class.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Ie
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Also supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Most browsers support this spelled as a prefixed `:-vendor-any()` pseudo-class.
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    
    , { browser = Safari
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Just """
          Also supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Only supports the `:-webkit-any()` pseudo-class, which is deprecated due to handling specificity incorrectly.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-media-interaction
-}
cssMediaInteraction : List BrowserSupport
cssMediaInteraction =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 41
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 28
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-media-resolution
-}
cssMediaResolution : List BrowserSupport
cssMediaResolution =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Supports the non-standard `min`/`max-device-pixel-ratio`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.3
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Supports the non-standard `min`/`max-device-pixel-ratio`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Supports the non-standard `min`/`max-device-pixel-ratio`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 29
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox before 16 supports only `dpi` unit, but you can set `2dppx` per `min--moz-device-pixel-ratio: 2`
          """
      , support = PartiallySupported
      , version = VersionNumber 3.5
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Ie
      , note = Just """
          Supports the `dpi` unit, but does not support `dppx` or `dpcm` units.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          Supports the `dpi` unit, but does not support `dppx` or `dpcm` units.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Supports the non-standard `min`/`max-device-pixel-ratio`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Supports the `dpi` unit, but does not support `dppx` or `dpcm` units.
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Supports the non-standard `min`/`max-device-pixel-ratio`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Just """
          Supports the non-standard `min`/`max-device-pixel-ratio`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 9.5 9.6
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Safari
      , note = Just """
          Supports the non-standard `min`/`max-device-pixel-ratio`
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-mediaqueries
-}
cssMediaqueries : List BrowserSupport
cssMediaqueries =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Does not support nested media queries
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Does not support nested media queries
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 26
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.5
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          Does not support nested media queries
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          Does not support nested media queries
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Does not support nested media queries
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.5 9.6
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Does not support nested media queries
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-mixblendmode
-}
cssMixblendmode : List BrowserSupport
cssMixblendmode =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled in Chrome through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 29
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 41
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 32
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial in Safari refers to not supporting the `hue`, `saturation`, `color`, and `luminosity` blend modes.
          """
      , support = PartiallySupported
      , version = VersionNumber 8
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 29
      }
    
    , { browser = Safari
      , note = Just """
          Partial in Safari refers to not supporting the `hue`, `saturation`, `color`, and `luminosity` blend modes.
          """
      , support = PartiallySupported
      , version = VersionNumber 7.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-motion-paths
-}
cssMotionPaths : List BrowserSupport
cssMotionPaths =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Requires the "Experimental Web Platform features" flag to be enabled
          """
      , support = NotSupported
      , version = VersionNumber 43
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 46
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Requires the "Experimental Web Platform features" flag to be enabled
          """
      , support = NotSupported
      , version = VersionNumber 30
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 33
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-namespaces
-}
cssNamespaces : List BrowserSupport
cssNamespaces =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.2 4.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-not-sel-list
-}
cssNotSelList : List BrowserSupport
cssNotSelList =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 65
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 59
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-nth-child-of
-}
cssNthChildOf : List BrowserSupport
cssNthChildOf =
    [ { browser = ChromeForAndroid
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Just """
          For support information for just `:nth-child()` see [CSS3 selector support](#feat=css-sel3)
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-opacity
-}
cssOpacity : List BrowserSupport
cssOpacity =
    [ { browser = ChromeForAndroid
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Transparency for elements in IE8 and older can be achieved using the proprietary "filter" property and does not work well with PNG images using alpha transparency.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-optional-pseudo
-}
cssOptionalPseudo : List BrowserSupport
cssOptionalPseudo =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.3
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Does not match non-required `<select>`s
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Does not match non-required `<select>`s
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Does not match non-required `<select>`s
          """
      , support = PartiallySupported
      , version = VersionRange 10.0 10.1
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-overflow-anchor
-}
cssOverflowAnchor : List BrowserSupport
cssOverflowAnchor =
    [ { browser = ChromeForAndroid
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = Supported
      , version = VersionNumber 56
      }
    
    , { browser = Edge
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = Supported
      , version = VersionNumber 43
      }
    
    , { browser = Safari
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Mozilla has discussed a similar feature in [Bug 43114](https://bugzilla.mozilla.org/show_bug.cgi?id=43114).
          """
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-overscroll-behavior
-}
cssOverscrollBehavior : List BrowserSupport
cssOverscrollBehavior =
    [ { browser = ChromeForAndroid
      , note = Just """
          Does not support `overscroll-behavior: none;` on the body element to prevent the overscroll glow and rubberbanding effects.
          """
      , support = PartiallySupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Does not support `overscroll-behavior: none;` on the body element to prevent the overscroll glow and rubberbanding effects.
          """
      , support = PartiallySupported
      , version = VersionNumber 63
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 65
      }
    
    , { browser = Edge
      , note = Just """
          Supports the precursor version of the property: [`-ms-scroll-chaining`](https://msdn.microsoft.com/en-us/library/hh772034%28v=vs.85%29.aspx), which works similarly. IE support is limited to Windows 8 & above
          """
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 59
      }
    
    , { browser = Ie
      , note = Just """
          Supports the precursor version of the property: [`-ms-scroll-chaining`](https://msdn.microsoft.com/en-us/library/hh772034%28v=vs.85%29.aspx), which works similarly. IE support is limited to Windows 8 & above
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Supports the precursor version of the property: [`-ms-scroll-chaining`](https://msdn.microsoft.com/en-us/library/hh772034%28v=vs.85%29.aspx), which works similarly. IE support is limited to Windows 8 & above
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Does not support `overscroll-behavior: none;` on the body element to prevent the overscroll glow and rubberbanding effects.
          """
      , support = PartiallySupported
      , version = VersionNumber 50
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-page-break
-}
cssPageBreak : List BrowserSupport
cssPageBreak =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Supports the `page-break-*` alias from the CSS 2.1 specification, but not the `break-*` properties from the latest spec.
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Supports the `page-break-*` alias from the CSS 2.1 specification, but not the `break-*` properties from the latest spec.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Not all mobile browsers offer print support; support listed for these is based on browser engine capability.
          """
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Supports the `page-break-*` alias from the CSS 2.1 specification, but not the `break-*` properties from the latest spec.
          """
      , support = Supported
      , version = VersionRange 10.0 10.1
      }
    
    , { browser = Opera
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    ]



{-| css-paint-api
-}
cssPaintApi : List BrowserSupport
cssPaintApi =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 65
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-placeholder
-}
cssPlaceholder : List BrowserSupport
cssPlaceholder =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = Edge
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox 18 and below supported the `:-moz-placeholder` pseudo-class rather than the `::-moz-placeholder` pseudo-element.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 19
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 51
      }
    
    , { browser = Ie
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 4.2 4.3
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 10.3
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 44
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 5
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = Supported
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support refers to using alternate names:
      `::-webkit-input-placeholder` for Chrome/Safari/Opera ([Chrome issue #623345](https://bugs.chromium.org/p/chromium/issues/detail?id=623345))
      `:-ms-input-placeholder` for IE. 
      `::-ms-input-placeholder` for Edge (also supports webkit prefix)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-placeholder-shown
-}
cssPlaceholderShown : List BrowserSupport
cssPlaceholderShown =
    [ { browser = ChromeForAndroid
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 47
      }
    
    , { browser = Edge
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 51
      }
    
    , { browser = Ie
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 34
      }
    
    , { browser = Safari
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Just """
          For support of styling the actual placeholder text itself, see [CSS ::placeholder](https://caniuse.com/#feat=css-placeholder)
          """
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-read-only-write
-}
cssReadOnlyWrite : List BrowserSupport
cssReadOnlyWrite =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Supports selector only for input and textarea fields, but not for contenteditable
          """
      , support = PartiallySupported
      , version = VersionNumber 2.3
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Supports selector only for input and textarea fields, but not for contenteditable
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Supports selector only for input and textarea fields, but not for contenteditable
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 13
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 3
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Supports selector only for input and textarea fields, but not for contenteditable
          """
      , support = PartiallySupported
      , version = VersionRange 4.2 4.3
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Supports selector only for input and textarea fields, but not for contenteditable
          """
      , support = PartiallySupported
      , version = VersionNumber 11.5
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Supports selector only for input and textarea fields, but not for contenteditable
          """
      , support = PartiallySupported
      , version = VersionNumber 11.5
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 23
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Supports selector only for input and textarea fields, but not for contenteditable
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-rebeccapurple
-}
cssRebeccapurple : List BrowserSupport
cssRebeccapurple =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 38
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 33
      }
    
    , { browser = Ie
      , note = Just """
          Only works in IE11 for Windows 10, not older versions of Windows
          """
      , support = PartiallySupported
      , version = VersionNumber 11
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 25
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-reflections
-}
cssReflections : List BrowserSupport
cssReflections =
    [ { browser = ChromeForAndroid
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Similar effect can be achieved in Firefox 4+ using the -moz-element() background property
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-regions
-}
cssRegions : List BrowserSupport
cssRegions =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 19
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 35
      }
    
    , { browser = Edge
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-repeating-gradients
-}
cssRepeatingGradients : List BrowserSupport
cssRepeatingGradients =
    [ { browser = ChromeForAndroid
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 26
      }
    
    , { browser = Edge
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.6
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Ie
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.1
      }
    
    , { browser = OperaMobile
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = OperaMobile
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.1
      }
    
    , { browser = Opera
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 11.6
      }
    
    , { browser = Opera
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Safari
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Firefox 10+, Chrome 26+ and Opera 11.6+ also support the new "to (side)" syntax.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-resize
-}
cssResize : List BrowserSupport
cssResize =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Presto-based Opera 12.10+ currently only supports the resize property for textarea elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-revert-value
-}
cssRevertValue : List BrowserSupport
cssRevertValue =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-rrggbbaa
-}
cssRrggbbaa : List BrowserSupport
cssRrggbbaa =
    [ { browser = ChromeForAndroid
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag.
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag.
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag.
          """
      , support = NotSupported
      , version = VersionNumber 52
      }
    
    , { browser = Chrome
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = Edge
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = Supported
      , version = VersionNumber 49
      }
    
    , { browser = Ie
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag.
          """
      , support = NotSupported
      , version = VersionNumber 39
      }
    
    , { browser = Safari
      , note = Just """
          Support in Android WebView is currently disabled due to [this issue](https://bugs.chromium.org/p/chromium/issues/detail?id=618472)
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Can be enabled via the "Experimental web platform features" flag.
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    ]



{-| css-scroll-behavior
-}
cssScrollBehavior : List BrowserSupport
cssScrollBehavior =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 41
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to support everything except of `Element.scrollIntoView()` and not together with pinch viewport.
          """
      , support = Supported
      , version = VersionNumber 61
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 28
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to support everything except of `Element.scrollIntoView()` and not together with pinch viewport.
          """
      , support = Supported
      , version = VersionNumber 48
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-scrollbar
-}
cssScrollbar : List BrowserSupport
cssScrollbar =
    [ { browser = ChromeForAndroid
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Currently scrollbar styling doesn't appear to be on any standards track.
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Currently scrollbar styling doesn't appear to be on any standards track.
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 2.3
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Currently scrollbar styling doesn't appear to be on any standards track.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Currently scrollbar styling doesn't appear to be on any standards track.
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          Only supports styling [scrollbar colors](https://msdn.microsoft.com/en-us/library/ms531155%28v=vs.85%29.aspx), no other properties to define the scrollbar's appearance.
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Currently scrollbar styling doesn't appear to be on any standards track.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Currently scrollbar styling doesn't appear to be on any standards track.
          """
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Currently scrollbar styling doesn't appear to be on any standards track.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Currently scrollbar styling doesn't appear to be on any standards track.
          """
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 5.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Supports scrollbar styling via CSS [pseudo-properties](https://webkit.org/blog/363/styling-scrollbars/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| css-sel2
-}
cssSel2 : List BrowserSupport
cssSel2 =
    [ { browser = ChromeForAndroid
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = NotSupported
      , version = VersionNumber 6
      }
    
    , { browser = Ie
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = IeMobile
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Support for `:visited` styling [varies across browsers](http://sixrevisions.com/css/visited-pseudo-class-strange/) due to security concerns.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-sel3
-}
cssSel3 : List BrowserSupport
cssSel3 =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.5
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 6
      }
    
    , { browser = Ie
      , note = Just """
          IE7 and IE8 support only these CSS3 selectors: General siblings (`element1~element2`) and Attribute selectors `[attr^=val]`, `[attr$=val]`, and `[attr*=val]`
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.5 9.6
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-selection
-}
cssSelection : List BrowserSupport
cssSelection =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.5
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.5 9.6
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-shapes
-}
cssShapes : List BrowserSupport
cssShapes =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Partially supported in Firefox by enabling "layout.css.shape-outside.enabled" in about:config
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled in Chrome through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 34
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partially supported in Firefox by enabling "layout.css.shape-outside.enabled" in about:config
          """
      , support = NotSupported
      , version = VersionNumber 51
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 8
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 24
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 7.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-snappoints
-}
cssSnappoints : List BrowserSupport
cssSnappoints =
    [ { browser = ChromeForAndroid
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Supports properties from an [older version](https://www.w3.org/TR/2015/WD-css-snappoints-1-20150326/) of the spec.
          """
      , support = PartiallySupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Available behind the "Experimental Web Platform features" feature flag
          """
      , support = NotSupported
      , version = VersionNumber 66
      }
    
    , { browser = Edge
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Supports properties from an [older version](https://www.w3.org/TR/2015/WD-css-snappoints-1-20150326/) of the spec.
          """
      , support = PartiallySupported
      , version = VersionNumber 39
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11
      }
    
    , { browser = IeMobile
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = Supported
      , version = VersionRange 11.0 11.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Works in the iOS WKWebView, but not UIWebView.
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css-sticky
-}
cssSticky : List BrowserSupport
cssSticky =
    [ { browser = ChromeForAndroid
      , note = Just """
          Supported on `th` elements, but not `thead` or `tr` - See [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=702927)
          """
      , support = PartiallySupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Not supported on any `table` parts - See [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=975644)
          """
      , support = PartiallySupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Enabled through the "experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Supported on `th` elements, but not `thead` or `tr` - See [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=702927)
          """
      , support = PartiallySupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Supported on `th` elements, but not `thead` or `tr` - See [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=702927)
          """
      , support = PartiallySupported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled through the "experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 23
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled through the "experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 52
      }
    
    , { browser = Chrome
      , note = Just """
          Supported on `th` elements, but not `thead` or `tr` - See [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=702927)
          """
      , support = PartiallySupported
      , version = VersionNumber 56
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Firefox
      , note = Just """
          Can be enabled in Firefox by setting the about:config preference layout.css.sticky.enabled to true
          """
      , support = NotSupported
      , version = VersionNumber 26
      }
    
    , { browser = Firefox
      , note = Just """
          Not supported on any `table` parts - See [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=975644)
          """
      , support = PartiallySupported
      , version = VersionNumber 32
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 59
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Do not appear to support sticky table headers
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 8
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Enabled through the "experimental Web Platform features" flag
          """
      , support = NotSupported
      , version = VersionNumber 39
      }
    
    , { browser = Opera
      , note = Just """
          Supported on `th` elements, but not `thead` or `tr` - See [Chrome bug](https://bugs.chromium.org/p/chromium/issues/detail?id=702927)
          """
      , support = PartiallySupported
      , version = VersionNumber 42
      }
    
    , { browser = Safari
      , note = Just """
          Do not appear to support sticky table headers
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 7.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 6.2
      }
    ]



{-| css-table
-}
cssTable : List BrowserSupport
cssTable =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox 2 does not support `inline-table`
          """
      , support = PartiallySupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-text-align-last
-}
cssTextAlignLast : List BrowserSupport
cssTextAlignLast =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled through the "Enable Experimental Web Platform Features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 35
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 47
      }
    
    , { browser = Edge
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 49
      }
    
    , { browser = Ie
      , note = Just """
          In Internet Explorer, the start and end values are not supported.
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          In Internet Explorer, the start and end values are not supported.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Enabled through the "Enable Experimental Web Platform Features" flag in opera://flags
          """
      , support = NotSupported
      , version = VersionNumber 22
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 34
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-text-indent
-}
cssTextIndent : List BrowserSupport
cssTextIndent =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 38
      }
    
    , { browser = Edge
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 25
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support refers to supporting a `<length>` value, but not the `each-line` or `hanging` keywords.
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 5
      }
    ]



{-| css-text-justify
-}
cssTextJustify : List BrowserSupport
cssTextJustify =
    [ { browser = ChromeForAndroid
      , note = Just """
          `inter-word` and `distribute` values supported behind the "Experimental platform features" flag but `distribute` support [is buggy](https://bugs.chromium.org/p/chromium/issues/detail?id=467406)
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Supports `auto`, `none`, `inter-word`, `inter-character`, and `distribute` with the exact same meaning and behavior as `inter-character` for legacy reasons.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          `inter-word` and `distribute` values supported behind the "Experimental platform features" flag but `distribute` support [is buggy](https://bugs.chromium.org/p/chromium/issues/detail?id=467406)
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          `inter-word` and `distribute` values supported behind the "Experimental platform features" flag but `distribute` support [is buggy](https://bugs.chromium.org/p/chromium/issues/detail?id=467406)
          """
      , support = NotSupported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          `inter-word` and `distribute` values supported behind the "Experimental platform features" flag but `distribute` support [is buggy](https://bugs.chromium.org/p/chromium/issues/detail?id=467406)
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          `inter-word` and `distribute` values supported behind the "Experimental platform features" flag but `distribute` support [is buggy](https://bugs.chromium.org/p/chromium/issues/detail?id=467406)
          """
      , support = NotSupported
      , version = VersionNumber 43
      }
    
    , { browser = Edge
      , note = Just """
          Supports `inter-word`, but not `inter-character` or  `none`. Also supports the following unofficial values: `distribute` , `distribute-all-lines`, `distribute-center-last`, `inter-cluster`, `inter-ideograph`, `newspaper`. See [MSDN](https://msdn.microsoft.com/en-us/library/ms531172%28v=vs.85%29.aspx) for details.
          """
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 54
      }
    
    , { browser = Firefox
      , note = Just """
          Supports `auto`, `none`, `inter-word`, `inter-character`, and `distribute` with the exact same meaning and behavior as `inter-character` for legacy reasons.
          """
      , support = Supported
      , version = VersionNumber 55
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          Supports `inter-word`, but not `inter-character` or  `none`. Also supports the following unofficial values: `distribute` , `distribute-all-lines`, `distribute-center-last`, `inter-cluster`, `inter-ideograph`, `newspaper`. See [MSDN](https://msdn.microsoft.com/en-us/library/ms531172%28v=vs.85%29.aspx) for details.
          """
      , support = PartiallySupported
      , version = VersionNumber 8
      }
    
    , { browser = IeMobile
      , note = Just """
          Supports `inter-word`, but not `inter-character` or  `none`. Also supports the following unofficial values: `distribute` , `distribute-all-lines`, `distribute-center-last`, `inter-cluster`, `inter-ideograph`, `newspaper`. See [MSDN](https://msdn.microsoft.com/en-us/library/ms531172%28v=vs.85%29.aspx) for details.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          `inter-word` and `distribute` values supported behind the "Experimental platform features" flag but `distribute` support [is buggy](https://bugs.chromium.org/p/chromium/issues/detail?id=467406)
          """
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          `inter-word` and `distribute` values supported behind the "Experimental platform features" flag but `distribute` support [is buggy](https://bugs.chromium.org/p/chromium/issues/detail?id=467406)
          """
      , support = NotSupported
      , version = VersionNumber 30
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          `inter-word` and `distribute` values supported behind the "Experimental platform features" flag but `distribute` support [is buggy](https://bugs.chromium.org/p/chromium/issues/detail?id=467406)
          """
      , support = NotSupported
      , version = VersionNumber 5
      }
    ]



{-| css-text-orientation
-}
cssTextOrientation : List BrowserSupport
cssTextOrientation =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 48
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 51
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 35
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-textshadow
-}
cssTextshadow : List BrowserSupport
cssTextshadow =
    [ { browser = ChromeForAndroid
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          IE 10+ supports a fourth length value for the shadow's "spread". This is not (yet) part of the specification.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 3.5
      }
    
    , { browser = Ie
      , note = Just """
          IE 10+ supports a fourth length value for the shadow's "spread". This is not (yet) part of the specification.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          IE 10+ supports a fourth length value for the shadow's "spread". This is not (yet) part of the specification.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionRange 9.5 9.6
      }
    
    , { browser = Safari
      , note = Just """
          Partial support in Safari 3.* refers to not supporting multiple shadows.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Opera Mini ignores the blur-radius set, so no blur effect is visible. Text-shadow behavior can be somewhat emulated in older IE versions using the non-standard "dropshadow" or "glow" filters.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-touch-action
-}
cssTouchAction : List BrowserSupport
cssTouchAction =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Supported in Firefox behind the `layout.css.touch_action.enabled` flag, Firefox for Windows 8 Touch ('Metro') enabled by default.
          """
      , support = NotSupported
      , version = VersionNumber 29
      }
    
    , { browser = Firefox
      , note = Just """
          Not applicable to Firefox platforms that support neither pointer nor touch events.
          """
      , support = Supported
      , version = VersionNumber 52
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = Ie
      , note = Just """
          IE10+ has already supported these property which are not in standard at present such as `double-tap-zoom`, `cross-slide-x`, `cross-slide-y`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IeMobile
      , note = Just """
          IE10+ has already supported these property which are not in standard at present such as `double-tap-zoom`, `cross-slide-x`, `cross-slide-y`.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IosSafari
      , note = Just """
          iOS Safari only supports `auto` and `manipulation`.
          """
      , support = PartiallySupported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 23
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-transitions
-}
cssTransitions : List BrowserSupport
cssTransitions =
    [ { browser = ChromeForAndroid
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 26
      }
    
    , { browser = Edge
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Does not support the `steps()`, `step-start` & `step-end` timing functions
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 5
      }
    
    , { browser = Firefox
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Ie
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Does not support the `steps()`, `step-start` & `step-end` timing functions
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = SupportedWithPrefix
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Does not support the `steps()`, `step-start` & `step-end` timing functions
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = OperaMobile
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Just """
          Does not support the `steps()`, `step-start` & `step-end` timing functions
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10.5
      }
    
    , { browser = Opera
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Opera
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Safari
      , note = Just """
          Does not support the `steps()`, `step-start` & `step-end` timing functions
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Support listed is for `transition` properties as well as the `transitionend` event.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-unset-value
-}
cssUnsetValue : List BrowserSupport
cssUnsetValue =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 41
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 13
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 27
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 28
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-variables
-}
cssVariables : List BrowserSupport
cssVariables =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled through the "Experimental Web Platform features" flag in `chrome://flags`
          """
      , support = NotSupported
      , version = VersionNumber 48
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 49
      }
    
    , { browser = Edge
      , note = Just """
          Partial support is due to bugs present (see known issues)
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 31
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Enabled through the "Experimental Web Platform features" flag in `chrome://flags`
          """
      , support = NotSupported
      , version = VersionNumber 35
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-widows-orphans
-}
cssWidowsOrphans : List BrowserSupport
cssWidowsOrphans =
    [ { browser = ChromeForAndroid
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 25
      }
    
    , { browser = Edge
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          Supports widows & orphans properties, but due to not supporting CSS multi-columns the support is only for page breaks (for print)
          """
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = Ie
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Just """
          Supports widows & orphans properties, but due to not supporting CSS multi-columns the support is only for page breaks (for print)
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Safari
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Some older WebKit-based browsers recognize the properties, but do not appear to have actual support
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css-writing-mode
-}
cssWritingMode : List BrowserSupport
cssWritingMode =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 3
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 8
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 48
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Supported in Firefox under the `layout.css.vertical-text.enabled` flag
          """
      , support = NotSupported
      , version = VersionNumber 36
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 41
      }
    
    , { browser = Ie
      , note = Just """
          Internet Explorer supports different values from an [earlier version of the spec](https://www.w3.org/TR/2003/CR-css3-text-20030514/#Progression), which originated from SVG.
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 11.0 11.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 35
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| css-zoom
-}
cssZoom : List BrowserSupport
cssZoom =
    [ { browser = ChromeForAndroid
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          The `-ms-zoom` property is an extension to CSS, and can be used as a synonym for `zoom` in IE8 Standards mode.
          """
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = IeMobile
      , note = Just """
          The `-ms-zoom` property is an extension to CSS, and can be used as a synonym for `zoom` in IE8 Standards mode.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Originally implemented only in Internet Explorer. Although several other browsers support the property, using `transform: scale()` is the recommended solution to scale content.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css3-attr
-}
css3Attr : List BrowserSupport
css3Attr =
    [ { browser = ChromeForAndroid
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          See the [generated content](/#feat=css-gencontent) table for support for `attr()` for the `content` property.
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css3-boxsizing
-}
css3Boxsizing : List BrowserSupport
css3Boxsizing =
    [ { browser = ChromeForAndroid
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Edge
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 29
      }
    
    , { browser = Ie
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = IeMobile
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionRange 9.5 9.6
      }
    
    , { browser = Safari
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 5.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Firefox versions before 57 also supported the `padding-box` value for `box-sizing`, though this value was been removed from the specification and later versions of the browser.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css3-colors
-}
css3Colors : List BrowserSupport
css3Colors =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = PartiallySupported
      , version = VersionRange 9.5 9.6
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionRange 10.0 10.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| css3-cursors
-}
css3Cursors : List BrowserSupport
css3Cursors =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = Edge
      , note = Just """
          Partial support refers to not supporting 'none'.
          """
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 14
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Ie
      , note = Just """
          Partial support refers to no support for the alias, cell, copy, ew-resize, ns-resize, nesw-resize, nwse-resize or context-menu cursors.
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to not supporting 'none'.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css3-cursors-newer
-}
css3CursorsNewer : List BrowserSupport
css3CursorsNewer =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 24
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.6
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 24
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| css3-tabsize
-}
css3Tabsize : List BrowserSupport
css3Tabsize =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupported
      , version = VersionNumber 4.4
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupported
      , version = VersionNumber 21
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 42
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 53
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupportedWithPrefix
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10.6
      }
    
    , { browser = Opera
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 29
      }
    
    , { browser = Safari
      , note = Just """
          Partial refers to supporting `<integer>` but not `<length>` values.
          """
      , support = PartiallySupported
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| currentcolor
-}
currentcolor : List BrowserSupport
currentcolor =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.5 9.6
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| devicepixelratio
-}
devicepixelratio : List BrowserSupport
devicepixelratio =
    [ { browser = ChromeForAndroid
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 18
      }
    
    , { browser = Ie
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IeMobile
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IosSafari
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Opera
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 11.6
      }
    
    , { browser = Safari
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          As the page is zoomed in the number of device pixels that one CSS pixel covers increases, and therefore the value of devicePixelRatio will also increase.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| flexbox
-}
flexbox : List BrowserSupport
flexbox =
    [ { browser = ChromeForAndroid
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Only supports the [old flexbox](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723) specification and does not support wrapping.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Only supports the [old flexbox](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723) specification and does not support wrapping.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Only supports the [old flexbox](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723) specification and does not support wrapping.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 21
      }
    
    , { browser = Chrome
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 29
      }
    
    , { browser = Edge
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Only supports the [old flexbox](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723) specification and does not support wrapping.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Just """
          Does not support flex-wrap, flex-flow or align-content properties
          """
      , support = PartiallySupported
      , version = VersionNumber 22
      }
    
    , { browser = Firefox
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 28
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Ie
      , note = Just """
          Partial support is due to large amount of bugs present (see known issues)
          """
      , support = PartiallySupported
      , version = VersionNumber 11
      }
    
    , { browser = IeMobile
      , note = Just """
          Only supports the [2012 syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/)
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IosSafari
      , note = Just """
          Only supports the [old flexbox](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723) specification and does not support wrapping.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = SupportedWithPrefix
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 17
      }
    
    , { browser = Safari
      , note = Just """
          Only supports the [old flexbox](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723) specification and does not support wrapping.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Most partial support refers to supporting an [older version](https://www.w3.org/TR/2009/WD-css3-flexbox-20090723/) of the specification or an [older syntax](https://www.w3.org/TR/2012/WD-css3-flexbox-20120322/).
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| flow-root
-}
flowRoot : List BrowserSupport
flowRoot =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 58
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 53
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 45
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| font-family-system-ui
-}
fontFamilySystemUi : List BrowserSupport
fontFamilySystemUi =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Supported as the 'BlinkMacSystemFont'  value (only on Mac)
          """
      , support = NotSupported
      , version = VersionNumber 53
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 56
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Supported as the `-apple-ui` value (only on Mac)
          """
      , support = PartiallySupported
      , version = VersionNumber 43
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Supported as the `-apple-ui` value (only on Mac)
          """
      , support = PartiallySupported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 11.0 11.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 43
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 9.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 6.2
      }
    ]



{-| font-feature
-}
fontFeature : List BrowserSupport
fontFeature =
    [ { browser = ChromeForAndroid
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support in older Chrome versions refers to lacking support in Mac OS X.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 16
      }
    
    , { browser = Chrome
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 21
      }
    
    , { browser = Chrome
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 48
      }
    
    , { browser = Edge
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          From Gecko 2.0 (Firefox 4.0) to Gecko 14.0 (Firefox 14.0) included, Gecko supported an older syntax, slightly different from the modern one: https://hacks.mozilla.org/2010/11/firefox-4-font-feature-support/
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Firefox
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 34
      }
    
    , { browser = Ie
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = NotSupported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 35
      }
    
    , { browser = Safari
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Safari
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = NotSupported
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Whenever possible, font-variant shorthand property or an associated longhand property, font-variant-ligatures, font-variant-caps, font-variant-east-asian, font-variant-alternates, font-variant-numeric or font-variant-position should be used. This property is a low-level feature designed to handle special cases where no other way to enable or access an OpenType font feature exists. In particular, this CSS property shouldn't be used to enable small caps.
          """
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| font-kerning
-}
fontKerning : List BrowserSupport
fontKerning =
    [ { browser = ChromeForAndroid
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionRange 4.43 4.44
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 29
      }
    
    , { browser = Chrome
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 33
      }
    
    , { browser = Edge
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Disabled by default, can be enabled using preference layout.css.font-features.enabled - defaulting to true on Nightly and Aurora only.
          """
      , support = NotSupported
      , version = VersionNumber 24
      }
    
    , { browser = Firefox
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 34
      }
    
    , { browser = Ie
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 8
      }
    
    , { browser = OperaMini
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 16
      }
    
    , { browser = Opera
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 20
      }
    
    , { browser = Safari
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Safari
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Browsers with support for [font feature settings](https://caniuse.com/#feat=font-feature) can also set kerning value.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| font-loading
-}
fontLoading : List BrowserSupport
fontLoading =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 35
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Can be enabled in Firefox using the `layout.css.font-loading-api.enabled` flag. Enabled by default in Firefox 41. See [this bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1149381)
          """
      , support = NotSupported
      , version = VersionNumber 35
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 41
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 22
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| font-size-adjust
-}
fontSizeAdjust : List BrowserSupport
fontSizeAdjust =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Does not appear to work on Firefox mobile, despite recognition of the property.
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Enabled through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 43
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Enabled through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 30
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| font-smooth
-}
fontSmooth : List BrowserSupport
fontSmooth =
    [ { browser = ChromeForAndroid
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 5
      }
    
    , { browser = Edge
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 25
      }
    
    , { browser = Ie
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Though present in early (2002) drafts of CSS3 Fonts, `font-smooth` has been removed from this specification and is currently not on the standard track.
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| font-unicode-range
-}
fontUnicodeRange : List BrowserSupport
fontUnicodeRange =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Edge
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Edge
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 17
      }
    
    , { browser = Firefox
      , note = Just """
          Can be enabled in Firefox using the `layout.css.unicode-range.enabled` flag
          """
      , support = NotSupported
      , version = VersionNumber 36
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 44
      }
    
    , { browser = Ie
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 23
      }
    
    , { browser = Safari
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support indicates that unnecessary code-ranges are downloaded by the browser - see [browser test matrix](https://docs.google.com/a/chromium.org/spreadsheets/d/18h-1gaosu4-KYxH8JUNL6ZDuOsOKmWfauoai3CS3hPY/edit?pli=1#gid=0).
          """
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| font-variant-alternates
-}
fontVariantAlternates : List BrowserSupport
fontVariantAlternates =
    [ { browser = ChromeForAndroid
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 16
      }
    
    , { browser = Edge
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Just """
          Experimental support available by enabling the layout.css.font-features.enabled flag
          """
      , support = NotSupported
      , version = VersionNumber 24
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 34
      }
    
    , { browser = Ie
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Low-level syntax available in [font-feature-settings](https://caniuse.com/#feat=font-feature) property equivalent to OpenType features: salt, ss01 through ss20, cv01 through cv99, swsh, cswh, ornm, nalt
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| fontface
-}
fontface : List BrowserSupport
fontface =
    [ { browser = ChromeForAndroid
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = PartiallySupported
      , version = VersionNumber 2.2
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 3.5
      }
    
    , { browser = Ie
      , note = Just """
          Partial support refers to only supporting EOT fonts.
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to only supporting SVG fonts.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionRange 4.2 4.3
      }
    
    , { browser = OperaMini
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionRange 10.0 10.1
      }
    
    , { browser = Safari
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Not supported by IE Mobile 9 and below.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| getcomputedstyle
-}
getcomputedstyle : List BrowserSupport
getcomputedstyle =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to not supporting getComputedStyle on pseudo-elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support refers to not supporting getComputedStyle on pseudo-elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to not supporting getComputedStyle on pseudo-elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to requiring the second parameter to be included.
          """
      , support = PartiallySupported
      , version = VersionNumber 3
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to not supporting getComputedStyle on pseudo-elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial support refers to not supporting getComputedStyle on pseudo-elements.
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to not supporting getComputedStyle on pseudo-elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to not supporting getComputedStyle on pseudo-elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.6
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to not supporting getComputedStyle on pseudo-elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| inline-block
-}
inlineBlock : List BrowserSupport
inlineBlock =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          Only supported in IE6 and IE7 on elements with a display of "inline" by default. [Alternative properties](http://blog.mozilla.com/webdev/2009/02/20/cross-browser-inline-block/) are available to provide complete cross-browser support.
          """
      , support = PartiallySupported
      , version = VersionNumber 6
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| intrinsic-width
-}
intrinsicWidth : List BrowserSupport
intrinsicWidth =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Prefixes are on the values, not the property names (e.g. -webkit-min-content)
      
      Older webkit browsers also support the unofficial `intrinsic` value which acts the same as `max-content`.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 22
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 46
      }
    
    , { browser = Edge
      , note = Just """
          Prefixes are on the values, not the property names (e.g. -webkit-min-content)
      
      Older webkit browsers also support the unofficial `intrinsic` value which acts the same as `max-content`.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3
      }
    
    , { browser = Ie
      , note = Just """
          Prefixes are on the values, not the property names (e.g. -webkit-min-content)
      
      Older webkit browsers also support the unofficial `intrinsic` value which acts the same as `max-content`.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Prefixes are on the values, not the property names (e.g. -webkit-min-content)
      
      Older webkit browsers also support the unofficial `intrinsic` value which acts the same as `max-content`.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Prefixes are on the values, not the property names (e.g. -webkit-min-content)
      
      Older webkit browsers also support the unofficial `intrinsic` value which acts the same as `max-content`.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 33
      }
    
    , { browser = Opera
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = Supported
      , version = VersionNumber 34
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 35
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = PartiallySupported
      , version = VersionNumber 11
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Does not support the `flex-basis` property. See [specs](https://www.w3.org/TR/2015/WD-css-flexbox-1-20150514/#flex-basis-property), [Blink bug](https://codereview.chromium.org/1304853002/), [Firefox bug](https://bugzilla.mozilla.org/show_bug.cgi?id=1055887).
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| kerning-pairs-ligatures
-}
kerningPairsLigatures : List BrowserSupport
kerningPairsLigatures =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support in Android browser versions is due to a serious bug where `text-rendering: optimizeLegibility` causes custom web fonts to not render.
          """
      , support = PartiallySupported
      , version = VersionNumber 3
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.2 4.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| minmaxwh
-}
minmaxwh : List BrowserSupport
minmaxwh =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = Ie
      , note = Just """
          IE7 does not support `inherit` as a value on any of these properties.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Ie
      , note = Just """
          IE8 has some bugs with `max-width`/`height` combined with `overflow: auto`/`scroll`.
          """
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| multibackgrounds
-}
multibackgrounds : List BrowserSupport
multibackgrounds =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.6
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.5
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| multicolumn
-}
multicolumn : List BrowserSupport
multicolumn =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 50
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupported
      , version = VersionNumber 52
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.1
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.1
      }
    
    , { browser = Opera
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to not supporting the `break-before`, `break-after`, `break-inside` properties. WebKit- and Blink-based browsers do have equivalent support for the non-standard `-webkit-column-break-*` properties to accomplish the same result (but only the `auto` and `always` values). Firefox does not support `break-*` but does support the `page-break-*` properties to accomplish the same result.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| object-fit
-}
objectFit : List BrowserSupport
objectFit =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.43 4.44
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 31
      }
    
    , { browser = Edge
      , note = Just """
          Partial support in Edge refers to `object-fit` only supporting `<img>` ([see this comment](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/13603873/#comment-0))
          """
      , support = PartiallySupported
      , version = VersionNumber 16
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support in Safari refers to support for `object-fit` but not `object-position`.
          """
      , support = PartiallySupported
      , version = VersionNumber 8
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 10.0 10.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = SupportedWithPrefix
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 11
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 10.6
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 19
      }
    
    , { browser = Safari
      , note = Just """
          Partial support in Safari refers to support for `object-fit` but not `object-position`.
          """
      , support = PartiallySupported
      , version = VersionNumber 7.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| outline
-}
outline : List BrowserSupport
outline =
    [ { browser = ChromeForAndroid
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Edge
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Just """
          Does not support `outline-offset`.
          """
      , support = PartiallySupported
      , version = VersionNumber 8
      }
    
    , { browser = Ie
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Does not support `outline-offset`.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Opera
      , note = Just """
          Does not support `outline-offset`.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 11.6
      }
    
    , { browser = Opera
      , note = Just """
          Also supports the value of `invert` for `outline-color`. (support of this value is optional for browsers)
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Firefox also supports the non-standard `-moz-outline-radius` property that acts similar to `border-radius`.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| pointer-events
-}
pointerEvents : List BrowserSupport
pointerEvents =
    [ { browser = ChromeForAndroid
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 3.6
      }
    
    , { browser = Ie
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IeMobile
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IosSafari
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Already part of the SVG specification, and all SVG-supporting browsers appear to support the property on SVG elements.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| prefers-reduced-motion
-}
prefersReducedMotion : List BrowserSupport
prefersReducedMotion =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| rem
-}
rem : List BrowserSupport
rem =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.6
      }
    
    , { browser = Ie
      , note = Just """
          IE 9 & IE 10 do not support `rem` units when used in the `font` shorthand property (the entire declaration is ignored) or when used on pseudo elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = IosSafari
      , note = Just """
          iOS Safari 5.0-5.1 support `rem` but not in combination with media queries.
          """
      , support = PartiallySupported
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.6
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| run-in
-}
runIn : List BrowserSupport
runIn =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionRange 4.43 4.44
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 32
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Not before inline-elements
          """
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 8
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 19
      }
    
    , { browser = Safari
      , note = Just """
          Not before inline-elements
          """
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| style-scoped
-}
styleScoped : List BrowserSupport
styleScoped =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Enabled in Firefox through the about:config setting "layout.css.scoped-style.enabled"
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled in Chrome through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 20
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 37
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 21
      }
    
    , { browser = Firefox
      , note = Just """
          Enabled in Firefox through the about:config setting "layout.css.scoped-style.enabled"
          """
      , support = NotSupported
      , version = VersionNumber 55
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 9
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| svg-css
-}
svgCss : List BrowserSupport
svgCss =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = Edge
      , note = Just """
          Partial support in Edge 15 and older refers to a lack of support for SVG data URIs. [see bug](https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/6274479/)
          """
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support in older Firefox and Opera Mini/Mobile refers to SVG images being blurry when scaled.
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 24
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support in iOS Safari and older Safari versions refers to failing to support tiling or the background-position property.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.2 4.3
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial support in older Firefox and Opera Mini/Mobile refers to SVG images being blurry when scaled.
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support in older Firefox and Opera Mini/Mobile refers to SVG images being blurry when scaled.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionRange 9.5 9.6
      }
    
    , { browser = Safari
      , note = Just """
          Partial support in iOS Safari and older Safari versions refers to failing to support tiling or the background-position property.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| text-decoration
-}
textDecoration : List BrowserSupport
textDecoration =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support refers to `text-decoration-skip` only supporting value `objects` and `ink`.
          """
      , support = PartiallySupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Partial support refers to `text-decoration-skip` only supporting value `objects` and `ink`.
          """
      , support = PartiallySupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Enabled in Chrome through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 26
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to `text-decoration-skip` only supporting value `objects` and `ink`.
          """
      , support = PartiallySupported
      , version = VersionNumber 57
      }
    
    , { browser = Edge
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to not supporting the `text-decoration-skip` property.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 6
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to not supporting the `text-decoration-skip` property.
          """
      , support = PartiallySupported
      , version = VersionNumber 36
      }
    
    , { browser = Ie
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to not supporting the `text-decoration-style` property.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 8
      }
    
    , { browser = OperaMini
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Enabled in Chrome through the "experimental Web Platform features" flag in chrome://flags
          """
      , support = NotSupported
      , version = VersionNumber 35
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to `text-decoration-skip` only supporting value `objects` and `ink`.
          """
      , support = PartiallySupported
      , version = VersionNumber 44
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 8
      }
    
    , { browser = SamsungInternet
      , note = Just """
          All browsers support the CSS2 version of `text-decoration`, which matches only the `text-decoration-line` values (`underline`, etc.)
          """
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| text-emphasis
-}
textEmphasis : List BrowserSupport
textEmphasis =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Some old WebKit browsers (like Chrome 24) support `-webkit-text-emphasis`, but does not support CJK languages and is therefore considered unsupported.
          """
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 25
      }
    
    , { browser = Edge
      , note = Just """
          Some old WebKit browsers (like Chrome 24) support `-webkit-text-emphasis`, but does not support CJK languages and is therefore considered unsupported.
          """
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Can be enabled in Firefox using the `layout.css.text-emphasis.enabled` flag
          """
      , support = NotSupported
      , version = VersionNumber 45
      }
    
    , { browser = Firefox
      , note = Just """
          Some old WebKit browsers (like Chrome 24) support `-webkit-text-emphasis`, but does not support CJK languages and is therefore considered unsupported.
          """
      , support = Supported
      , version = VersionNumber 46
      }
    
    , { browser = Ie
      , note = Just """
          Some old WebKit browsers (like Chrome 24) support `-webkit-text-emphasis`, but does not support CJK languages and is therefore considered unsupported.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Some old WebKit browsers (like Chrome 24) support `-webkit-text-emphasis`, but does not support CJK languages and is therefore considered unsupported.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Some old WebKit browsers (like Chrome 24) support `-webkit-text-emphasis`, but does not support CJK languages and is therefore considered unsupported.
          """
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Some old WebKit browsers (like Chrome 24) support `-webkit-text-emphasis`, but does not support CJK languages and is therefore considered unsupported.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 6.1
      }
    
    , { browser = Safari
      , note = Just """
          Some old WebKit browsers (like Chrome 24) support `-webkit-text-emphasis`, but does not support CJK languages and is therefore considered unsupported.
          """
      , support = Supported
      , version = VersionNumber 7.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support refers to incorrect support for `-webkit-text-emphasis-position`. These browsers support `over` and `under` as values, but not the added `left` and `right` values required by the spec.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| text-overflow
-}
textOverflow : List BrowserSupport
textOverflow =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.1
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Ie
      , note = Nothing
      , support = Supported
      , version = VersionNumber 6
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = Supported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 9
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| text-size-adjust
-}
textSizeAdjust : List BrowserSupport
textSizeAdjust =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Old versions of WebKit-based desktop browsers (Chrome<27, Safari<6) [suffer from a bug](https://bugs.webkit.org/show_bug.cgi?id=56543) where if `-webkit-text-size-adjust` is explicitly set to `none`, instead of ignoring the property, the browsers will prevent the user from zooming in or out on the webpage.
          """
      , support = NotSupported
      , version = VersionNumber 26
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 27
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 54
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          If the viewport size is set using a `<meta>` element, the `-ms-text-size-adjust` property is ignored. See [MSDN](https://msdn.microsoft.com/en-us/library/ie/dn793579%28v=vs.85%29.aspx)
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionRange 5.0 5.1
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 43
      }
    
    , { browser = Opera
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 44
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 45
      }
    
    , { browser = Safari
      , note = Just """
          Old versions of WebKit-based desktop browsers (Chrome<27, Safari<6) [suffer from a bug](https://bugs.webkit.org/show_bug.cgi?id=56543) where if `-webkit-text-size-adjust` is explicitly set to `none`, instead of ignoring the property, the browsers will prevent the user from zooming in or out on the webpage.
          """
      , support = NotSupported
      , version = VersionNumber 5.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 6
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 5
      }
    ]



{-| text-stroke
-}
textStroke : List BrowserSupport
textStroke =
    [ { browser = ChromeForAndroid
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Firefox & Edge specifically only support the `-webkit-text-stroke` property (not using `-moz-` or `-ms-` prefix)
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = NotSupported
      , version = VersionNumber 3
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Just """
          Firefox & Edge specifically only support the `-webkit-text-stroke` property (not using `-moz-` or `-ms-` prefix)
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 48
      }
    
    , { browser = Firefox
      , note = Just """
          Firefox & Edge specifically only support the `-webkit-text-stroke` property (not using `-moz-` or `-ms-` prefix)
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 49
      }
    
    , { browser = Ie
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = PartiallySupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionRange 4.0 4.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Does not yet appear in any W3C specification. Was briefly included in a spec as the "text-outline" property, but this was removed.
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| transforms2d
-}
transforms2D : List BrowserSupport
transforms2D =
    [ { browser = ChromeForAndroid
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Edge
      , note = Just """
          Does not support CSS transforms on SVG elements (transform attribute can be used instead)
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.5
      }
    
    , { browser = Firefox
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Ie
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = NotSupported
      , version = VersionNumber 6
      }
    
    , { browser = Ie
      , note = Just """
          Does not support CSS transforms on SVG elements (transform attribute can be used instead)
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 9
      }
    
    , { browser = Ie
      , note = Just """
          Does not support CSS transforms on SVG elements (transform attribute can be used instead)
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = Opera
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 10.5
      }
    
    , { browser = Opera
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Opera
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 23
      }
    
    , { browser = Safari
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = SupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Just """
          The scale transform can be emulated in IE < 9 using Microsoft's "zoom" extension, others are (not easily) possible using the MS Matrix filter
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| transforms3d
-}
transforms3D : List BrowserSupport
transforms3D =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 3
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Ie
      , note = Just """
          Partial support in IE refers to not supporting [the transform-style: preserve-3d property](http://msdn.microsoft.com/en-us/library/ie/hh673529%28v=vs.85%29.aspx#the_ms_transform_style_property). This prevents nesting 3D transformed elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Partial support in IE refers to not supporting [the transform-style: preserve-3d property](http://msdn.microsoft.com/en-us/library/ie/hh673529%28v=vs.85%29.aspx#the_ms_transform_style_property). This prevents nesting 3D transformed elements.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Safari 9, 10 & 11 are reported to still require a prefix for the related `backface-visibility` property.
          """
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 23
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Safari
      , note = Just """
          Safari 9, 10 & 11 are reported to still require a prefix for the related `backface-visibility` property.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| ttf
-}
ttf : List BrowserSupport
ttf =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 2.2
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.5
      }
    
    , { browser = Ie
      , note = Just """
          Partial support in IE9+ refers to the fonts only working [when set to be "installable"](http://blogs.msdn.com/b/ie/archive/2010/07/15/the-css-corner-better-web-typography-for-better-design.aspx). Support for this is tracked [here](https://status.modern.ie/crossdomainfontloading).
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = IeMobile
      , note = Just """
          Partial support in IE9+ refers to the fonts only working [when set to be "installable"](http://blogs.msdn.com/b/ie/archive/2010/07/15/the-css-corner-better-web-typography-for-better-design.aspx). Support for this is tracked [here](https://status.modern.ie/crossdomainfontloading).
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 4.2 4.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionRange 10.0 10.1
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| user-select-none
-}
userSelectNone : List BrowserSupport
userSelectNone =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 54
      }
    
    , { browser = Edge
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 2
      }
    
    , { browser = Ie
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 3.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 41
      }
    
    , { browser = Safari
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 3.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = SupportedWithPrefix
      , version = VersionNumber 4
      }
    ]



{-| variable-fonts
-}
variableFonts : List BrowserSupport
variableFonts =
    [ { browser = ChromeForAndroid
      , note = Just """
          Does not support the `font-weight` and `font-stretch` properties, nor `format('truetype-variations')`
          """
      , support = PartiallySupported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Does not support the `font-weight` and `font-stretch` properties, nor `format('truetype-variations')`
          """
      , support = PartiallySupported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Works with Experimental Web Platform features enabled
          """
      , support = NotSupported
      , version = VersionNumber 59
      }
    
    , { browser = Chrome
      , note = Just """
          Does not support the `font-weight` and `font-stretch` properties, nor `format('truetype-variations')`
          """
      , support = PartiallySupported
      , version = VersionNumber 62
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 17
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 53
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionRange 11.0 11.2
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = Opera
      , note = Just """
          Does not support the `font-weight` and `font-stretch` properties, nor `format('truetype-variations')`
          """
      , support = PartiallySupported
      , version = VersionNumber 49
      }
    
    , { browser = Safari
      , note = Just """
          Requires MacOS 10.13+
          """
      , support = Supported
      , version = VersionNumber 11
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 4
      }
    ]



{-| viewport-units
-}
viewportUnits : List BrowserSupport
viewportUnits =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to not supporting the "vmax" unit. 
          """
      , support = PartiallySupported
      , version = VersionNumber 20
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 26
      }
    
    , { browser = Edge
      , note = Just """
          Partial support refers to not supporting the "vmax" unit. 
          """
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Edge
      , note = Nothing
      , support = Supported
      , version = VersionNumber 16
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 19
      }
    
    , { browser = Ie
      , note = Just """
          Partial support in IE9 refers to supporting "vm" instead of "vmin".
          """
      , support = PartiallySupported
      , version = VersionNumber 9
      }
    
    , { browser = Ie
      , note = Just """
          Partial support refers to not supporting the "vmax" unit. 
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IeMobile
      , note = Just """
          Partial support refers to not supporting the "vmax" unit. 
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = PartiallySupported
      , version = VersionRange 6.0 6.1
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support in iOS7 is due to buggy behavior of the "vh" unit (see workarounds: [1](https://gist.github.com/pburtchaell/e702f441ba9b3f76f587), [2](https://gist.github.com/BenMorel/e9e34c08360ebbbd0634)).
          """
      , support = PartiallySupported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 8
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to not supporting the "vmax" unit. 
          """
      , support = PartiallySupported
      , version = VersionNumber 6
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| will-change
-}
willChange : List BrowserSupport
willChange =
    [ { browser = ChromeForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Nothing
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Nothing
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Nothing
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Edge
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Supported in Firefox behind the `layout.css.will-change.enabled` flag
          """
      , support = NotSupported
      , version = VersionNumber 29
      }
    
    , { browser = Firefox
      , note = Nothing
      , support = Supported
      , version = VersionNumber 36
      }
    
    , { browser = Ie
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Nothing
      , support = NotSupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.3
      }
    
    , { browser = OperaMini
      , note = Nothing
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Nothing
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Nothing
      , support = Supported
      , version = VersionNumber 24
      }
    
    , { browser = Safari
      , note = Nothing
      , support = Supported
      , version = VersionNumber 9.1
      }
    
    , { browser = SamsungInternet
      , note = Nothing
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| word-break
-}
wordBreak : List BrowserSupport
wordBreak =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = PartiallySupported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = PartiallySupported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = PartiallySupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 62
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 44
      }
    
    , { browser = Edge
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 15
      }
    
    , { browser = Ie
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionRange 9.0 9.2
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = NotSupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = PartiallySupported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = PartiallySupported
      , version = VersionNumber 15
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 31
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 9
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support refers to supporting the `break-all` value, but not the `keep-all` value.
      
      Chrome, Safari and other WebKit/Blink browsers also support the unofficial `break-word` value which is treated like `word-wrap: break-word`.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]



{-| wordwrap
-}
wordwrap : List BrowserSupport
wordwrap =
    [ { browser = ChromeForAndroid
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 64
      }
    
    , { browser = FirefoxForAndroid
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 57
      }
    
    , { browser = QqBrowser
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 1.2
      }
    
    , { browser = UcBrowserForAndroid
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 11.8
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 2.1
      }
    
    , { browser = AndroidBrowser
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 4.4
      }
    
    , { browser = BaiduBrowser
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 7.12
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 7
      }
    
    , { browser = BlackberryBrowser
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 10
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 4
      }
    
    , { browser = Chrome
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 23
      }
    
    , { browser = Edge
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 12
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.5
      }
    
    , { browser = Firefox
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 49
      }
    
    , { browser = Ie
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 5.5
      }
    
    , { browser = IeMobile
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.2
      }
    
    , { browser = IosSafari
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionRange 7.0 7.1
      }
    
    , { browser = OperaMini
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = AllVersions
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 10
      }
    
    , { browser = OperaMobile
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 37
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 10.5
      }
    
    , { browser = Opera
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 12.1
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = PartiallySupported
      , version = VersionNumber 3.1
      }
    
    , { browser = Safari
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 6.1
      }
    
    , { browser = SamsungInternet
      , note = Just """
          Partial support refers to requiring the legacy name "word-wrap" (rather than "overflow-wrap") to work.
          """
      , support = Supported
      , version = VersionNumber 4
      }
    ]
