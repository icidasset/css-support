## CSS.Support

A combination of the CSS-related data from [caniuse.com](https://caniuse.com/) and all the standardized CSS properties. Find out what the browser support is for CSS properties, check if a browser has support for a CSS property, etc.



### How to use

```elm
import Css.Support.Data exposing (Browser(Chrome), Version(..))


Css.Support.forTarget "flex" (Chrome, VersionNumber 22)
--> SupportedWithPrefix


Css.Support.compatible "flex" { includePartialSupport = True }
--> Just
-->   [
-->     { browser = Chrome
-->     , note = ..., support = PartiallySupportedWithPrefix
-->     , version = VersionNumber 4
-->     }
-->   , { browser = Chrome,
-->     , note = ...
-->     , support = SupportedWithPrefix
-->     , version = VersionNumber 21
-->     }
-->   , { browser = Chrome
-->     , note = ...
-->     , support = Supported
-->     , version = VersionNumber 29
-->     }
-->   ]


Css.Support.Data.prefixFor Chrome
--> "webkit"
```

More information can be found in the [documentation](http://package.elm-lang.org/packages/icidasset/css-support/latest).



### Development

```shell
brew install node
brew install haskell-stack
brew install elm@0.18

stack build :generator
stack exec generator

npm test

elm-make --docs=documentation.json
```
