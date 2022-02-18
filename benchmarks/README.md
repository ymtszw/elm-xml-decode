# Benchmark app

See [elm-explorations/benchmark](https://github.com/elm-explorations/benchmark)

```sh
# In this directory (if elm is globally available):
$ elm make DecodeString.elm --optimize --output=../docs/DecodeString.html
$ elm make DecodeXml.elm --optimize --output=../docs/DecodeXml.html
```

Then, open `../docs/index.html` in your browser!


## Result History

### Elm 0.19.1, elm-xml-decode 3.0

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

### Elm 0.18, elm-xml-decode 1.x

Was using [BrianHicks/elm-benchmark](https://github.com/BrianHicks/elm-benchmark).

Sample result (on my MacBookPro early 2015):

- CPU: Core i5 2.7GHz
- Mem: DDR3 8GB 1867MHz
- macOS (was El Capitan era, IIRC)
- Google Chrome 63.0.3239.84 64bit

![bench 1.0](https://raw.githubusercontent.com/ymtszw/elm-xml-decode/master/benchmarks/result1.0.png)
