module DecodeXml exposing (main)

import Benchmark exposing (benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import DecodeString exposing (cdCatalogDecoder, noteDecoder)
import ExampleXml
import Xml.Decode exposing (..)
import XmlParser


main : BenchmarkProgram
main =
    program <|
        let
            justRootXml =
                unsafeParse ExampleXml.justRoot

            noteXml =
                unsafeParse ExampleXml.note

            cdCatalogXml =
                unsafeParse ExampleXml.cdCatalog
        in
        describe "Xml.Decode.decodeXml"
            [ benchmark "just root" <|
                \_ -> decodeXml (succeed ()) justRootXml
            , benchmark "note" <|
                \_ -> decodeXml noteDecoder noteXml
            , benchmark "CD catalog" <|
                \_ -> decodeXml cdCatalogDecoder cdCatalogXml
            ]


unsafeParse : String -> XmlParser.Xml
unsafeParse xmlStr =
    case XmlParser.parse xmlStr of
        Ok xml ->
            xml

        Err _ ->
            -- Should not happen
            unsafeParse xmlStr
