module IssueCases exposing (suite)

import Expect
import Test exposing (..)
import Xml.Decode exposing (..)


suite : Test
suite =
    describe "From Issues"
        [ describe "#4" <|
            -- https://github.com/ymtszw/elm-xml-decode/issues/4
            let
                exampleXml =
                    "<root><ListItem id=\"1\"><value>a</value></ListItem><ListItem id=\"2\"><value>b</value></ListItem></root>"
            in
            [ test "incorrect decoder" <|
                \_ ->
                    let
                        listItemDecoder =
                            map2 Tuple.pair
                                (path [] (single (intAttr "id")))
                                (path [ "value" ] (single string))
                    in
                    exampleXml
                        |> run (path [ "ListItem" ] (list listItemDecoder))
                        |> Expect.equal (Err "Path: /ListItem\nNode: <value>a</value>\nAttribute 'id' not found.")
            , test "fixed decoder" <|
                \_ ->
                    let
                        listItemDecoderFixed =
                            map2 Tuple.pair
                                (intAttr "id")
                                (path [ "value" ] (single string))
                    in
                    exampleXml
                        |> run (path [ "ListItem" ] (list listItemDecoderFixed))
                        |> Expect.equal (Ok [ ( 1, "a" ), ( 2, "b" ) ])
            ]
        ]
