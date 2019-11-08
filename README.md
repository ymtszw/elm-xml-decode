# elm-xml-decode

[![CircleCI][cc]](https://circleci.com/gh/ymtszw/elm-xml-decode/tree/master)

[cc]: https://circleci.com/gh/ymtszw/elm-xml-decode/tree/master.svg?style=svg

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

Globally install Elm Platform, [`elm-test`](https://github.com/rtfeldman/node-test-runner),
and [`elm-verify-examples`](https://github.com/stoeffel/elm-verify-examples)

```sh
$ elm-verify-examples
$ elm-test
```

## Benchmark: Are they efficient? Are they fast?

Benchmark codes can be found in `benchmarks/` directory.
Using [examples in W3School](https://www.w3schools.com/xml/xml_examples.asp).

```sh
$ cd benchmarks/
$ elm make Benchmarks.elm --optimize --output=../docs/index.html
```

Looking forward to see similar benchmarks from other related works!!

### In Elm 0.19

Using [elm-explorations/benchmark](https://github.com/elm-explorations/benchmark).
Available [here](https://ymtszw.github.io/elm-xml-decode/) as a static web page.

**It may hang for a while** during JIT warming up, but keep waiting (~ a minute).

Sample result (on my MacBook Pro 2019):

- Environment
  - CPU: Intel Core i7 1.7GHz
  - Mem: DDR4 16GB
  - macOS Mojave 10.14.6
  - Google Chrome 78.0.3904.87 64bit
- Versions
  - elm 0.19.1
  - elm-xml-decode version: 3.1.0
  - elm-benchmark 1.0.1

![bench20191108](https://raw.githubusercontent.com/ymtszw/elm-xml-decode/master/benchmarks/result20191108.png)

### In Elm 0.18 (elm-xml-decode 1.x)

Using [BrianHicks/elm-benchmark](https://github.com/BrianHicks/elm-benchmark).

Sample result (on my MacBookPro early 2015):

- CPU: Core i5 2.7GHz
- Mem: DDR3 8GB 1867MHz
- macOS (was El Capitan era, IIRC)
- Google Chrome 63.0.3239.84 64bit

![bench 1.0](https://raw.githubusercontent.com/ymtszw/elm-xml-decode/master/benchmarks/result1.0.png)

## License

BSD-3-Clause
