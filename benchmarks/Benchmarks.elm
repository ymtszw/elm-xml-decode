module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import ExampleXml
import Xml.Decode as XD
import Xml.Decode.Extra exposing ((|:))
import Xml.Decode.Internal as Internal
import XmlParser


suite : Benchmark
suite =
    describe "Components of elm-xml-decode"
        [ describe "Parsing minimal XML"
            [ benchmark "elm-xml-parser (using elm-tools/parser)" <|
                \_ -> XmlParser.parse ExampleXml.justRoot
            ]
        , Benchmark.compare "Escape CDATA for error dump"
            "escape (char-based)"
            (\_ -> Internal.escape escapableString)
            "escapeR (regex-based)"
            (\_ -> Internal.escapeR escapableString)
        , describe "Decode actual XmlParser.Xml"
            [ benchmark "decode note.xml (small)" <|
                \_ -> XD.decodeXml noteDecoder noteXml
            , benchmark "decode cd_catalog.xml (large)" <|
                \_ -> XD.decodeXml (XD.path [ "CD" ] (XD.list cdCatalogDecoder)) cdCatalogXml
            ]
        ]


escapableString : String
escapableString =
    " '<ab&cd>' "


noteDecoder : XD.Decoder ( String, String, String, String )
noteDecoder =
    XD.succeed (\a b c d -> ( a, b, c, d ))
        |: XD.path [ "to" ] (XD.single XD.string)
        |: XD.path [ "from" ] (XD.single XD.string)
        |: XD.path [ "heading" ] (XD.single XD.string)
        |: XD.path [ "body" ] (XD.single XD.string)


noteXml : XmlParser.Xml
noteXml =
    parse ExampleXml.note


type alias CD =
    ( String, String, String, String, Float, Int )


cdCatalogDecoder : XD.Decoder CD
cdCatalogDecoder =
    XD.succeed (\a b c d e f -> ( a, b, c, d, e, f ))
        |: XD.path [ "TITLE" ] (XD.single XD.string)
        |: XD.path [ "ARTIST" ] (XD.single XD.string)
        |: XD.path [ "COUNTRY" ] (XD.single XD.string)
        |: XD.path [ "COMPANY" ] (XD.single XD.string)
        |: XD.path [ "PRICE" ] (XD.single XD.float)
        |: XD.path [ "YEAR" ] (XD.single XD.int)


cdCatalogXml : XmlParser.Xml
cdCatalogXml =
    parse ExampleXml.cdCatalog


parse : String -> XmlParser.Xml
parse xmlStr =
    case XmlParser.parse xmlStr of
        Ok xml ->
            xml

        Err _ ->
            Debug.crash "Should not happen"


main : BenchmarkProgram
main =
    program suite
