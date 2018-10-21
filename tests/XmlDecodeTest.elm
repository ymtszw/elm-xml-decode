module XmlDecodeTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Xml.Decode exposing (..)
import Xml.Decode.Internal exposing (escape)


stringSuite : Test
stringSuite =
    describe "string"
        [ fuzzOk escapedStringFuzzer string "bare string" identity
        , testOk string "" ""
        , testErr string "<innerTag>nestedText</innerTag>"
        , testErr string "notSimpleText<innerTag></innerTag></tag>"
        , testErr string "<innerTag></innerTag>notSimpleText</tag>"
        ]


escapedStringFuzzer : Fuzzer String
escapedStringFuzzer =
    Fuzz.map escape Fuzz.string


testOk : Decoder a -> String -> a -> Test
testOk decoder input expect =
    test ("should decode " ++ xml input) <| \_ -> run decoder (xml input) |> Expect.equal (Ok expect)


testErr : Decoder a -> String -> Test
testErr decoder input =
    test ("should reject " ++ xml input) <| \_ -> run decoder (xml input) |> Expect.err


fuzzOk : Fuzzer a -> Decoder b -> String -> (a -> String) -> Test
fuzzOk fuzzer decoder valKind valConv =
    fuzz fuzzer ("should decode " ++ valKind) <| \v -> run decoder (xml <| valConv <| v) |> Expect.ok


fuzzErr : Fuzzer a -> Decoder b -> String -> (a -> String) -> Test
fuzzErr fuzzer decoder valKind valConv =
    fuzz fuzzer ("should reject " ++ valKind) <| \v -> run decoder (xml <| valConv <| v) |> Expect.err


xml : String -> String
xml contents =
    "<root>" ++ contents ++ "</root>"


intSuite : Test
intSuite =
    describe "int"
        [ fuzzOk Fuzz.int int "bare int" String.fromInt
        , testErr int "string"
        , testErr int "1.0"
        ]


floatSuite : Test
floatSuite =
    describe "float"
        [ fuzzOk Fuzz.float float "bare float" String.fromFloat
        , fuzzOk Fuzz.int float "bare int" String.fromInt
        , testErr float "string"
        ]


boolSuite : Test
boolSuite =
    describe "bool"
        [ testOk bool "true" True
        , testOk bool "1" True
        , testOk bool "false" False
        , testOk bool "0" False
        , fuzzErr nonBoolFuzzer bool "non-bool bare string" identity
        ]


nonBoolFuzzer : Fuzzer String
nonBoolFuzzer =
    let
        alterBoolString a =
            if List.member a [ "true", "1", "false", "0" ] then
                "a" ++ a

            else
                a
    in
    Fuzz.map alterBoolString escapedStringFuzzer


pathSuite : Test
pathSuite =
    describe "path"
        [ singleSuite
        , listSuite
        , leakyListSuite
        ]


singleSuite : Test
singleSuite =
    describe "single"
        [ testSinglePathOk [] "target"
        , testSinglePathOk [ "tag" ] "<tag>target</tag>"
        , testSinglePathOk [ "tag1", "tag2" ] "<tag1><tag2>target</tag2></tag1>"
        , testSinglePathOk [ "sametag", "sametag" ] "<sametag><sametag>target</sametag></sametag>"
        , testSinglePathErr [ "nonexisting" ] "<tag1><tag2>target</tag2></tag1>"
        , testSinglePathErr [ "tag1", "wrongLeaf" ] "<tag1><tag2>target</tag2></tag1>"
        , testSinglePathErr [ "wrongTrunk", "tag2" ] "<tag1><tag2>target</tag2></tag1>"
        , testSinglePathErr [ "multiple" ] "<multiple>target</multiple><multiple>target</multiple>"
        ]


testSinglePathOk : List String -> String -> Test
testSinglePathOk path_ doc =
    test ("should decode " ++ xml doc ++ " with path: " ++ Debug.toString path_) <|
        \_ ->
            run (path path_ (single string)) (xml doc)
                |> Expect.equal (Ok "target")


testSinglePathErr : List String -> String -> Test
testSinglePathErr path_ doc =
    test ("should fail to decode " ++ xml doc ++ " with path: " ++ Debug.toString path_) <|
        \_ ->
            run (path path_ (single string)) (xml doc)
                |> Expect.err


listSuite : Test
listSuite =
    describe "list"
        [ testListPathOk [ "tag" ] "<tag>value1</tag><tag>value2</tag>" [ "value1", "value2" ]
        , testListPathOk [ "tag" ] "<tag>nonEmpty</tag><tag></tag>" [ "nonEmpty", "" ]
        , testListPathOk [ "tag" ] "<tag>value1</tag>" [ "value1" ]
        , testListPathOk [ "tag" ] "" []
        , test "should fail to decode into list when value is not decodable" <|
            \_ ->
                run (path [ "tag" ] (list int)) (xml "<tag>nonInt</tag>") |> Expect.err
        ]


testListPathOk : List String -> String -> List String -> Test
testListPathOk path_ doc expect =
    test ("should decode " ++ xml doc ++ " with path: " ++ Debug.toString path_) <|
        \_ ->
            run (path path_ (list string)) (xml doc)
                |> Expect.equal (Ok expect)


leakyListSuite : Test
leakyListSuite =
    describe "leakyList"
        [ testLeakyListPathOk [ "tag" ] "<tag>1</tag><tag>2</tag>" [ 1, 2 ]
        , testLeakyListPathOk [ "tag" ] "<tag>1</tag><tag>nonInt</tag>" [ 1 ]
        , testLeakyListPathOk [ "tag" ] "<tag>nonInt</tag><tag>nonInt</tag>" []
        , testLeakyListPathOk [ "tag" ] "" []
        ]


testLeakyListPathOk : List String -> String -> List Int -> Test
testLeakyListPathOk path_ doc expect =
    test ("should decode " ++ xml doc ++ " with path: " ++ Debug.toString path_) <|
        \_ ->
            run (path path_ (leakyList int)) (xml doc)
                |> Expect.equal (Ok expect)


withDefaultSuite : Test
withDefaultSuite =
    describe "withDefault"
        [ testWithDefault bool "nonBool" True
        , testWithDefault (path [ "nonexisting" ] (single string)) "<tag>value</tag>" "default"
        ]


testWithDefault : Decoder a -> String -> a -> Test
testWithDefault decoder input expect =
    test ("should decode into default value from input " ++ xml input) <|
        \_ ->
            run (withDefault expect decoder) (xml input)
                |> Expect.equal (Ok expect)


maybeSuite : Test
maybeSuite =
    describe "maybe"
        [ testMaybe bool "true" (Just True)
        , testMaybe bool "nonBool" Nothing
        , testMaybe (path [ "nonexisting" ] (single string)) "<tag>value</tag>" Nothing
        ]


testMaybe : Decoder a -> String -> Maybe a -> Test
testMaybe decoder input expect =
    test ("should decode into " ++ Debug.toString expect ++ " from " ++ xml input) <|
        \_ ->
            run (maybe decoder) (xml input)
                |> Expect.equal (Ok expect)


suite : Test
suite =
    describe "Xml.Decode"
        [ stringSuite
        , intSuite
        , floatSuite
        , boolSuite
        , pathSuite
        , withDefaultSuite
        , maybeSuite
        ]
