# elm-xml-decode

[![GitHub package version][v]](http://package.elm-lang.org/packages/ymtszw/elm-xml-decode/latest)
[![license][l]](https://github.com/ymtszw/elm-xml-decode/blob/master/LICENSE)
[![Build Status][b]](https://travis-ci.org/ymtszw/elm-xml-decode)

[v]: https://img.shields.io/badge/dynamic/json.svg?label=elm-package&colorB=5f9ea0&query=$.version&uri=https%3A%2F%2Fraw.githubusercontent.com%2Fymtszw%2Felm-xml-decode%2Fmaster%2Felm-package.json
[l]: https://img.shields.io/badge/dynamic/json.svg?label=license&colorB=008b8b&query=$.license&uri=https%3A%2F%2Fraw.githubusercontent.com%2Fymtszw%2Felm-xml-decode%2Fmaster%2Felm-package.json
[b]: https://travis-ci.org/ymtszw/elm-xml-decode.svg?branch=master

XML decoder, sharing the spirit of [`Json.Decode`][jd].

Using [jinjor/elm-xml-parser][exp] as its parser component.

[jd]: http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode
[exp]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest

## Related Works

[eeue56/elm-xml][ex] is an existing full-package XML parser/decoder for Elm,
though I intend to provide an alternative XML decoder which exhibits following properties:

- Provides [`Decoder`][de]-based APIs, sharing the spirit of [`Json.Decode`][jd]
- Also provides DSL-styled decoder compositions, as in [`Xml.Decode.Pipeline`][xdp], [`Xml.Decode.Extra`][xde]
    - They share the sprits of [`Json.Decode.Pipeline`][jdp] and [`Json.Decode.Extra`][jde], respectively
- Handles list of XML node with identical tags, using [`ListDecoder`][ld] type
- Locates targeting XML nodes using "path" of tags, partially mimicking XPath

[ex]: http://package.elm-lang.org/packages/eeue56/elm-xml/latest
[de]: http://package.elm-lang.org/packages/ymtszw/elm-xml-decode/latest/Xml-Decode#Decoder
[jdp]: http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/Json-Decode-Pipeline
[jde]: http://package.elm-lang.org/packages/elm-community/json-extra/latest/Json-Decode-Extra
[xdp]: http://package.elm-lang.org/packages/ymtszw/elm-xml-decode/latest/Xml-Decode-Pipeline
[xde]: http://package.elm-lang.org/packages/ymtszw/elm-xml-decode/latest/Xml-Decode-Extra
[xdp]: http://package.elm-lang.org/packages/ymtszw/elm-xml-decode/latest/Xml-Decode#ListDecoder


## Examples

Basics:

```elm
import Xml.Decode exposing (Decoder, map2, path, single, list, string, int)


type alias Example =
    { string : String
    , integers : List Int
    }


exampleDecoder : Decoder Example
exampleDecoder =
    map2 Example
        (path [ "path", "to", "string", "value" ] (single string))
        (path [ "path", "to", "int", "values" ] (list int))
```

Execute decode:

```elm
Xml.Decode.run exampleDecoder
    """
    <root>
        <path>
            <to>
                <string>
                    <value>SomeString</value>
                </string>
                <int>
                    <values>1</values>
                    <values>2</values>
                </int>
            </to>
        </path>
    </root>
    """
--> Ok { string = "SomeString", integers = [ 1, 2 ] } : Example
```

### Decoder compositions

We have `map`, `map2` and variants, though the following methods are also possible.

Pipeline style!:

```elm
import Xml.Decode exposing (Decoder, succeed, single, list, string, int)
import Xml.Decode.Pipeline exposing (requiredPath)


exampleDecoder : Decoder Example
exampleDecoder =
    succeed Example
        |> requiredPath [ "path", "to", "string", "value" ] (single string)
        |> requiredPath [ "path", "to", "int", "values" ] (list int)
```

Applicative style!:

```elm
import Xml.Decode exposing (Decoder, succeed, path, single, list, string, int)
import Xml.Decode.Extra exposing ((|:))

exampleDecoder : Decoder Example
exampleDecoder =
    succeed Example
        |: path [ "path", "to", "string", "value" ] (single string)
        |: path [ "path", "to", "int", "values" ] (list int)
```

## Development

```sh
npm install
npm test
npm run watch:test # Monitor file changes and update doc-test
npm run docs       # Compile and generate docs.json
npm run analyse    # Analyse Elm code
```

## Are they efficient? Are they fast?

Benchmark and refinements/optimizations would be cool... though I haven't committed to them.
Help, help me!

## License

BSD-3-Clause
