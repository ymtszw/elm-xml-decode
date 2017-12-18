module Benchmarks exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)


suite : Benchmark
suite =
    let
        longString =
            String.repeat 100 "abcdefgh"

        mapper char =
            case char of
                'd' ->
                    'D'

                _ ->
                    char
    in
        describe "String"
            [ describe "map"
                [ benchmark "replace characters in a long string" <|
                    \_ -> String.map mapper longString
                ]
            ]


main : BenchmarkProgram
main =
    program suite
