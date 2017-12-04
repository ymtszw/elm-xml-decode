module XmlDecodeTest exposing (..)

import Expect exposing (..)
import Fuzz exposing (..)
import Test exposing (..)
import XmlParser exposing (Node(..))
import Xml.Decode as XD exposing (Decoder)


suite : Test
suite =
    describe "`Decode`"
        [ describe "`Decoder`s and `decodeXml`"
            [ describe "`string`"
                [ fuzz string "should decode string" <|
                    \str ->
                        [ Text str
                        , Element "tag" [] [ Text str ]
                        ]
                            |> List.map xml
                            |> List.map (XD.decodeXml XD.string)
                            |> mustBeAllOk str
                , test "should reject `Element` with non-singular children or non-`Text` child" <|
                    \_ ->
                        [ Element "tag" [] []
                        , Element "tag" [] [ Element "innerTag" [] [ Text "nestedText" ] ]
                        , Element "tag" [] [ Text "multiple", Element "elements" [] [] ]
                        , Element "tag" [] [ Element "multiple" [] [], Text "elements" ]
                        ]
                            |> List.map xml
                            |> List.map (XD.decodeXml XD.string)
                            |> mustBeAllError
                ]
            , describe "`int`"
                [ fuzz int "should decode integer" <|
                    \i ->
                        [ Text <| toString i
                        , Element "tag" [] [ Text <| toString i ]
                        ]
                            |> List.map xml
                            |> List.map (XD.decodeXml XD.int)
                            |> mustBeAllOk i
                , test "should reject non-integer" <|
                    \_ ->
                        [ Text "string"
                        , Text "1.0"
                        ]
                            |> List.map xml
                            |> List.map (XD.decodeXml XD.int)
                            |> mustBeAllError
                ]
            , describe "`float`"
                [ fuzz2 int float "should decode float (or int into float)" <|
                    \i f ->
                        [ Text <| toString i
                        , Text <| toString f
                        , Element "tag" [] [ Text <| toString i ]
                        , Element "tag" [] [ Text <| toString f ]
                        ]
                            |> List.map xml
                            |> List.map (XD.decodeXml XD.float)
                            |> equalLists
                                [ Ok (toFloat i)
                                , Ok f
                                , Ok (toFloat i)
                                , Ok f
                                ]
                , test "should reject non-float-convertible" <|
                    \_ ->
                        Text "string"
                            |> xml
                            |> XD.decodeXml XD.float
                            |> err
                ]
            , describe "`bool`"
                [ test "should decode boolean" <|
                    \_ ->
                        [ Text "true"
                        , Text "1"
                        , Text "false"
                        , Text "0"
                        , Element "tag" [] [ Text "true" ]
                        ]
                            |> List.map xml
                            |> List.map (XD.decodeXml XD.bool)
                            |> equalLists
                                [ Ok True
                                , Ok True
                                , Ok False
                                , Ok False
                                , Ok True
                                ]
                , fuzz (stringWithout [ "true", "false", "0", "1" ]) "should reject non-boolean" <|
                    \nonBool ->
                        Text nonBool
                            |> xml
                            |> XD.decodeXml XD.bool
                            |> err
                ]
            , describe "`path` with `single` and `list`"
                [ describe "`single`"
                    [ test "should extract value from root element" <|
                        \_ ->
                            Element "root" [] [ Text "value" ]
                                |> xml
                                |> XD.decodeXml (XD.path [] <| XD.single XD.string)
                                |> equal (Ok "value")
                    , test "should extract value from targeted node" <|
                        \_ ->
                            [ ( Element "root" [] [ Element "tag" [] [ Text "value" ] ], [ "tag" ] )
                            , ( Element "root" [] [ Element "tag1" [] [ Element "tag2" [] [ Text "value" ] ] ], [ "tag1", "tag2" ] )
                            , ( Element "root" [] [ Element "sametag" [] [ Element "sametag" [] [ Text "value" ] ] ], [ "sametag", "sametag" ] )
                            ]
                                |> List.map (\( node, path ) -> ( xml node, XD.path path <| XD.single XD.string ))
                                |> List.map (uncurry <| flip XD.decodeXml)
                                |> mustBeAllOk "value"
                    , test "should fail for paths that do not yield nodes" <|
                        \_ ->
                            [ [ "nonexisting" ]
                            , [ "tag1", "wrongLeaf" ]
                            , [ "wrongTrunk", "tag2" ]
                            ]
                                |> List.map (flip XD.path <| XD.single XD.string)
                                |> List.map (flip XD.decodeXml <| xml <| Element "root" [] [ Element "tag1" [] [ Element "tag2" [] [ Text "value" ] ] ])
                                |> mustBeAllError
                    , test "should fail for path with multiple matching nodes" <|
                        \_ ->
                            [ Element "tag" [] [ Text "value1" ]
                            , Element "tag" [] [ Text "value2" ]
                            ]
                                |> (xml << Element "root" [])
                                |> XD.decodeXml (XD.path [ "tag" ] <| XD.single XD.string)
                                |> err
                    ]
                , describe "`list`"
                    [ test "should extract values from targeted nodes" <|
                        \_ ->
                            [ [ Element "tag" [] [ Text "value1" ], Element "tag" [] [ Text "value2" ] ]
                            , [ Element "tag" [] [ Text "value1" ] ]
                            , []
                            ]
                                |> List.map (xml << Element "root" [])
                                |> List.map (XD.decodeXml <| XD.path [ "tag" ] <| XD.list XD.string)
                                |> equalLists
                                    [ Ok [ "value1", "value2" ]
                                    , Ok [ "value1" ]
                                    , Ok []
                                    ]
                    ]
                ]
            , describe "`withDefault`"
                [ test "should handle value decoding failure" <|
                    \_ ->
                        Text "nonBool"
                            |> xml
                            |> XD.decodeXml (XD.withDefault True XD.bool)
                            |> equal (Ok True)
                , test "should handle path finding failure" <|
                    \_ ->
                        Element "root" [] [ Element "tag" [] [ Text "value" ] ]
                            |> xml
                            |> XD.decodeXml (XD.withDefault "default" <| XD.path [ "nonexisting" ] <| XD.single XD.string)
                            |> equal (Ok "default")
                ]
            , describe "`maybe`"
                [ test "should handle value decoding failure" <|
                    \_ ->
                        [ Text "true"
                        , Text "nonBool"
                        ]
                            |> List.map xml
                            |> List.map (XD.decodeXml <| XD.maybe XD.bool)
                            |> equalLists
                                [ Ok (Just True)
                                , Ok Nothing
                                ]
                , test "should handle path finding failure" <|
                    \_ ->
                        Element "root" [] [ Element "tag" [] [ Text "value" ] ]
                            |> xml
                            |> XD.decodeXml (XD.maybe <| XD.path [ "nonexisting" ] <| XD.single XD.string)
                            |> equal (Ok Nothing)
                ]
            ]
        ]


xml : Node -> XmlParser.Xml
xml rootNode =
    { processingInstructions = []
    , docType = Nothing
    , root = rootNode
    }


stringWithout : List String -> Fuzzer String
stringWithout excluded =
    flip conditional
        string
        { retries = 5
        , fallback = always ""
        , condition = flip List.member excluded >> not
        }


mustBeAllOk : b -> List (Result a b) -> Expectation
mustBeAllOk val results =
    if List.all ((==) (Ok val)) results then
        pass
    else
        fail <| toString results


mustBeAllError : List (Result a b) -> Expectation
mustBeAllError results =
    let
        isErr res =
            case res of
                Ok _ ->
                    False

                Err _ ->
                    True
    in
        if List.all isErr results then
            pass
        else
            fail <| toString results
