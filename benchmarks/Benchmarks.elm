module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import ExampleXml
import Xml.Decode exposing (..)
import XmlParser


suite : Benchmark
suite =
    describe "Xml.Decode"
        [ describe "decodeString"
            [ benchmark "note" <|
                \_ -> decodeString noteDecoder ExampleXml.note
            , benchmark "cdCatalog" <|
                \_ -> decodeString cdCatalogDecoder ExampleXml.cdCatalog
            ]
        , describe "decodeXml"
            [ benchmark "note" <|
                \_ -> decodeXml noteDecoder noteXml
            , benchmark "cdCatalog" <|
                \_ -> decodeXml cdCatalogDecoder cdCatalogXml
            ]
        ]


type alias Note =
    { to : String
    , from : String
    , heading : String
    , body : String
    }


noteDecoder : Decoder Note
noteDecoder =
    map4 Note
        (path [ "to" ] (single string))
        (path [ "from" ] (single string))
        (path [ "heading" ] (single string))
        (path [ "body" ] (single string))


noteXml : XmlParser.Xml
noteXml =
    parse ExampleXml.note


type alias CD =
    { title : String
    , artist : String
    , country : String
    , company : String
    , price : Float
    , year : Int
    }


cdCatalogDecoder : Decoder (List CD)
cdCatalogDecoder =
    let
        cdDecoder =
            succeed CD
                |> andMap (path [ "TITLE" ] (single string))
                |> andMap (path [ "ARTIST" ] (single string))
                |> andMap (path [ "COUNTRY" ] (single string))
                |> andMap (path [ "COMPANY" ] (single string))
                |> andMap (path [ "PRICE" ] (single float))
                |> andMap (path [ "YEAR" ] (single int))
    in
    path [ "CD" ] (list cdDecoder)


cdCatalogXml : XmlParser.Xml
cdCatalogXml =
    parse ExampleXml.cdCatalog


parse : String -> XmlParser.Xml
parse xmlStr =
    case XmlParser.parse xmlStr of
        Ok xml ->
            xml

        Err _ ->
            -- Should not happen
            parse xmlStr


main : BenchmarkProgram
main =
    program suite
