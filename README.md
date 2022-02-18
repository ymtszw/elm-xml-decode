# elm-xml-decode

[![Main Workflow](https://github.com/ymtszw/elm-xml-decode/actions/workflows/main.yml/badge.svg)](https://github.com/ymtszw/elm-xml-decode/actions/workflows/main.yml)

XML decoder, sharing the spirit of [`Json.Decode`][jd]. Ready for Elm 0.19.

Using [jinjor/elm-xml-parser][exp] as its parser component, which is based on [elm/parser][ep].

[jd]: https://github.com/elm/json
[exp]: http://github.com/jinjor/elm-xml-parser
[ep]: https://github.com/elm/parser

## Related Works

[eeue56/elm-xml][ex] was an existing full-package XML parser/decoder for Elm,
though I intended to provide an alternative XML decoder which exhibits following properties:

- Provides [`Decoder`][de]-based APIs, sharing the spirit of [`Json.Decode`][jd]
- Also provides DSL-styled decoder compositions, sharing the sprits of [`Json.Decode.Pipeline`][jdp]
- Handles list of XML node with identical tags, using [`ListDecoder`][ld] type
- Locates targeting XML nodes using "path" of tags, partially mimicking XPath

[ex]: https://github.com/eeue56/elm-xml
[de]: https://package.elm-lang.org/packages/ymtszw/elm-xml-decode/latest/Xml-Decode#Decoder
[jdp]: https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/Json-Decode-Pipeline
[ld]: https://package.elm-lang.org/packages/ymtszw/elm-xml-decode/latest/Xml-Decode#ListDecoder

## Examples

Basics:

```elm
import Xml.Decode exposing (..)

type alias Data =
    { string : String
    , integers : List Int
    }

dataDecoder : Decoder Data
dataDecoder =
    map2 Data
        (path [ "path", "to", "string", "value" ] (single string))
        (path [ "path", "to", "int", "values" ] (list int))

run dataDecoder
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
--> Ok { string = "SomeString", integers = [ 1, 2 ] }
```

### Pipeline Decoder compositions

We have `map`, `map2` and variants, though the Pipeline style is also possible:

```elm
pipelineDecoder : Decoder Data
pipelineDecoder =
    succeed Data
        |> requiredPath [ "path", "to", "string", "value" ] (single string)
        |> requiredPath [ "path", "to", "int", "values" ] (list int)
```

## Development

Install reasonably new Node.js (currently [Node.js 16 is tested](https://github.com/ymtszw/elm-xml-decode/blob/master/.github/workflows/main.yml))

```sh
$ npm ci
$ npm test
```

## Benchmarks

Benchmark app can be found in `benchmarks/` directory.
Using [examples in W3School](https://www.w3schools.com/xml/xml_examples.asp) and
[elm-explorations/benchmark](https://github.com/elm-explorations/benchmark).

```sh
$ npm run generate-benchmark
# Open docs/index.html
```

Also available at <https://ymtszw.github.io/elm-xml-decode/>

**It may hang for a while** during JIT warming up, but keep waiting (~ a minute).

### Elm 0.19.1, elm-xml-decode 3.2.1

Sample result (on my Windows 10 machine):

- Environment
  - CPU: Intel Core i7-8700K @ 3.7GHz
  - Mem: 64GB
  - Windows 10 Pro, 10.0.19044
  - Google Chrome 98.0.4758.82 64bit
- Versions
  - elm 0.19.1
  - elm-xml-decode version: 3.2.1
  - elm-benchmark 1.0.2

![bench20220219](https://raw.githubusercontent.com/ymtszw/elm-xml-decode/master/benchmarks/result20220219.png)

## License

BSD-3-Clause
