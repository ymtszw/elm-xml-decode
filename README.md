# elm-xml-decode

[![GitHub package version][v]](http://package.elm-lang.org/packages/ymtszw/elm-xml-decode/latest)
[![license][l]](https://github.com/ymtszw/elm-xml-decode/blob/master/LICENSE)
[![TravisCI][tc]](https://travis-ci.org/ymtszw/elm-xml-decode)
[![CircleCI][cc]](https://circleci.com/gh/ymtszw/elm-xml-decode/tree/master)

[v]: https://img.shields.io/badge/elm--package-1.0.0-blue.svg?maxAge=3600
[l]: https://img.shields.io/badge/license-BSD--3--Clause-blue.svg?maxAge=3600
[tc]: https://travis-ci.org/ymtszw/elm-xml-decode.svg?branch=master
[cc]: https://circleci.com/gh/ymtszw/elm-xml-decode/tree/master.svg?style=svg

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
[ld]: http://package.elm-lang.org/packages/ymtszw/elm-xml-decode/latest/Xml-Decode#ListDecoder


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

## Benchmark: Are they efficient? Are they fast?

Benchmark codes can be found in `benchmarks/` directory.
Using [BrianHicks/elm-benchmark][eb] and [examples in W3School][w3s].
Available [here][bench] as a static web page.

[eb]: http://package.elm-lang.org/packages/BrianHicks/elm-benchmark/latest/Benchmark
[bench]: https://ymtszw.github.io/elm-xml-decode/
[w3s]: https://www.w3schools.com/xml/xml_examples.asp

Develop:

```sh
npm run bench -- init
npm run bench -- compile
open docs/index.html
```

Sample result (on my MacBookPro early 2015):

- CPU: Core i5 2.7GHz
- Mem: DDR3 8GB 1867MHz
- Google Chrome 63.0.3239.84 64bit

![bench](./benchmark.png)

[jinjor/elm-xml-parser][exp] is using [elm-tools/parser][etp],
which currently suffers from some performance issues (possibly related to [this comment][issue]?).
We hope the underlying issue will be resolved in future versions of Elm and/or those dependencies.

[etp]: http://package.elm-lang.org/packages/elm-tools/parser/latest
[issue]: https://github.com/elm-tools/parser/issues/15#issuecomment-336223879

Although decoding part should practically be fast enough (see decode result of note.xml and cd_catalog.xml).

Looking forward to see similar benchmarks from other related works!!

## License

BSD-3-Clause
