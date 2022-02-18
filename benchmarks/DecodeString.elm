module DecodeString exposing (cdCatalogDecoder, main, noteDecoder)

import Benchmark exposing (benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import ExampleXml
import Xml.Decode exposing (..)


main : BenchmarkProgram
main =
    program <|
        describe "Xml.Decode.decodeString"
            [ benchmark "just root" <|
                \_ -> decodeString (succeed ()) ExampleXml.justRoot
            , benchmark "note" <|
                \_ -> decodeString noteDecoder ExampleXml.note
            , benchmark "CD catalog" <|
                \_ -> decodeString cdCatalogDecoder ExampleXml.cdCatalog
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
