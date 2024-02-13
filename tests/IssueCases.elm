module IssueCases exposing (suite)

import Expect
import Test exposing (..)
import Xml.Decode exposing (..)
import XmlParser exposing (Node(..))


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
        , describe "discourse#5412" <|
            -- https://discourse.elm-lang.org/t/using-xml-decode/5412
            let
                exampleXml =
                    """
<Schema>
    <EntityType Name="Person">
        <Property>foo</Property>
        <Property>bar</Property>
    </EntityType>
    <ComplexType Name="Location">
        <Property>foo</Property>
        <Property>bar</Property>
    </ComplexType>
    <EntityType Name="Animal">
        <Property>ban</Property>
    </EntityType>
</Schema>
"""
            in
            [ test "proposedDecoder" <|
                \_ ->
                    let
                        proposedDecoder =
                            path [] (leakyList decodeSchemaEntry)

                        decodeSchemaEntry =
                            with node <|
                                \n ->
                                    case n of
                                        Element "EntityType" _ _ ->
                                            map2 EntityType (stringAttr "Name") decodeEntityTypeEntry

                                        Element "ComplexType" _ _ ->
                                            succeed ComplexType

                                        _ ->
                                            fail "More To Come"

                        decodeEntityTypeEntry =
                            oneOf
                                [ path [ "Property" ] <| leakyList <| map Property string

                                -- More to come here
                                ]
                    in
                    exampleXml
                        |> run proposedDecoder
                        |> Expect.equal
                            (Ok
                                [ EntityType "Person" [ Property "foo", Property "bar" ]
                                , ComplexType
                                , EntityType "Animal" [ Property "ban" ]
                                ]
                            )
            ]
        , describe "#8" <|
            -- https://github.com/ymtszw/elm-xml-decode/issues/8
            [ test "stringAttr should not cut values" <|
                \_ ->
                    "<root url=\"a&amp;b\"></root>"
                        |> run (stringAttr "url")
                        |> Expect.equal (Ok "a&b")
            ]
        ]



------------------------
--- For "discourse#5412"
------------------------


type SchemaEntry
    = EntityType String (List EntityTypeEntry)
    | ComplexType


type EntityTypeEntry
    = Property PropertyDetails


type alias PropertyDetails =
    String
