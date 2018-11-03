module Xml.Decode exposing
    ( Decoder, ListDecoder, Error(..)
    , run, decodeString, decodeXml
    , string, int, float, bool
    , stringAttr, intAttr, floatAttr, boolAttr
    , single, list, leakyList, index
    , succeed, fail, oneOf, andThen, map, map2, map3, map4, map5, andMap, withDefault, maybe, lazy
    , path
    , requiredPath, optionalPath, possiblePath
    , errorToString
    )

{-| XML decoder module sharing the spirit of [`Json.Decode`][jd].

Using [`XmlParser`][xp] as its parser component.

[jd]: http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode
[xp]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser


# Basic Example

Examples in this package are doc-tested.

    exampleDecoder : Decoder ( String, List Int )
    exampleDecoder =
        map2 Tuple.pair
            (path [ "string", "value" ] (single string))
            (path [ "int", "values" ] (list int))

    run exampleDecoder
        """
        <root>
            <string>
                <value>SomeString</value>
            </string>
            <int>
                <values>1</values>
                <values>2</values>
            </int>
        </root>
        """
    --> Ok ( "SomeString", [ 1, 2 ] )


# Types

@docs Decoder, ListDecoder, Error


# Decode Executor

@docs run, decodeString, decodeXml


# Decoders

@docs string, int, float, bool


# Attribute Decoders

@docs stringAttr, intAttr, floatAttr, boolAttr


# List Decoders

@docs single, list, leakyList, index


# Decoder Utilities

`mapN` series are backed by `Result.mapN` series, thus it only supports up to `map5`.

@docs succeed, fail, oneOf, andThen, map, map2, map3, map4, map5, andMap, withDefault, maybe, lazy


# Node Locater

@docs path


# Pipeline APIs

Allow writing Decoders in Pipeline-style, just like [`Json.Decode.Pipeline`][jdp].

[jdp]: http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/Json-Decode-Pipeline

@docs requiredPath, optionalPath, possiblePath


# Error Utility

@docs errorToString

-}

import Parser
import Xml.Decode.Internal exposing (..)
import XmlParser exposing (Attribute, Node(..), Xml)



-- TYPES


{-| A function that knows how to decode an XML node into Elm value.
-}
type Decoder a
    = Decoder (Node -> Result Error a)


{-| A function that knows how to decode list of XML nodes into Elm value.

They are constructed by functions such ad [`single`](#single) or [`list`](#list),
then used in conjunction with [`path`](#path).

See [`path`](#path) for examples.

-}
type ListDecoder a
    = ListDecoder (( List Node, Node ) -> Result Error a)


{-| Represents error on decode execution.
-}
type Error
    = Path (List String) Error
    | OneOf (List Error)
    | Failure String Node



-- DECODE EXECUTOR


{-| Synonym of [`decodeString`](#decodeString).
-}
run : Decoder a -> String -> Result String a
run =
    decodeString


{-| Parses an XML string and decodes into Elm value.

Parsing will be done by [`XmlParser.parse`][xpp]

This function returns error in `String`.
Should you need to separately handle [`Parser.Error`][pe] and [`Xml.Decode.Error`](#Error),
explicitly use [`XmlParser.parse`][xpp] and [`decodeXml`](#decodeXml).

[xpp]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser#parse
[pe]: http://package.elm-lang.org/packages/elm-tools/parser/latest/Parser#Error

-}
decodeString : Decoder a -> String -> Result String a
decodeString decoder s =
    case XmlParser.parse s of
        Ok xml ->
            case decodeXml decoder xml of
                Ok decoded ->
                    Ok decoded

                Err dErr ->
                    Err (errorToString dErr)

        Err pErr ->
            Err (parseErrorsToString pErr)


parseErrorsToString : List { advanced | row : Int, col : Int, problem : Parser.Problem } -> String
parseErrorsToString deadEnds =
    deadEnds
        |> List.map
            (\deadEnd ->
                ("At [" ++ String.fromInt deadEnd.row ++ "," ++ String.fromInt deadEnd.col ++ "], ")
                    ++ parserProblemToString deadEnd.problem
            )
        |> String.join "\n"
        |> String.append "Invalid XML document.\n"


parserProblemToString : Parser.Problem -> String
parserProblemToString problem =
    case problem of
        Parser.Expecting expect ->
            "I was expecting: " ++ expect

        Parser.ExpectingInt ->
            "I was expecting an integer"

        Parser.ExpectingHex ->
            "I was expecting a hexadecimal"

        Parser.ExpectingOctal ->
            "I was expecting an octal"

        Parser.ExpectingBinary ->
            "I was expecting a binary"

        Parser.ExpectingFloat ->
            "I was expecting a float"

        Parser.ExpectingNumber ->
            "I was expecting a number"

        Parser.ExpectingVariable ->
            "I was expecting a variable"

        Parser.ExpectingSymbol symbol ->
            "I was expecting a symbol: " ++ symbol

        Parser.ExpectingKeyword keyword ->
            "I was expecting a keyword: " ++ keyword

        Parser.ExpectingEnd ->
            "I was expecting the end of input"

        Parser.UnexpectedChar ->
            "I got an unexpected character"

        Parser.Problem text ->
            text

        Parser.BadRepeat ->
            "I got a bad repetition"


{-| Decodes an [`XmlParser.Xml`][xpx] value into other type of Elm value.

[xpx]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser#Xml

It discards Document Type Definitoin (DTD) and Processing Instruction in XML,
only cares about root XML node.

    import XmlParser exposing (Xml, Node(..))

    exampleDecoder : Decoder ( String, List Int )
    exampleDecoder =
        map2 Tuple.pair
            (path [ "string", "value" ] (single string))
            (path [ "int", "values" ] (list int))

    decodeXml exampleDecoder <|
        Xml [] Nothing <|
            Element "root" []
                [ Element "string" []
                    [ Element "value" [] [ Text "SomeString" ]
                    ]
                , Element "int" []
                    [ Element "values" [] [ Text "1" ]
                    , Element "values" [] [ Text "2" ]
                    ]
                ]
    --> Ok ( "SomeString", [ 1, 2 ] )

-}
decodeXml : Decoder a -> Xml -> Result Error a
decodeXml (Decoder decoder) { root } =
    decoder root



-- DECODERS


{-| Decodes an [`XmlParser.Node`][xpn] into `String`.

  - If the node is `XmlParser.Text`, extracts its value.
  - If the node is `XmlParser.Element` AND contains nothing, treat it as "empty text".
  - If the node is `XmlParser.Element` AND contains a single `XmlParser.Text` child,
    extracts its value.
  - Otherwise fails.

If you want to extract values from node attribute, use [`stringAttr`](#stringAttr) and variants.

[xpn]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser#Node

    run string "<root>string</root>"
    --> Ok "string"

    run string "<root></root>"
    --> Ok ""

    run string "<root><nested>string</nested></root>"
    --> Err "Node: <root><nested>string</nested></root>\nThe node is not a simple text node."

-}
string : Decoder String
string =
    cdata Ok


cdata : (String -> Result String a) -> Decoder a
cdata generator =
    Decoder (cdataImpl generator)


cdataImpl : (String -> Result String a) -> Node -> Result Error a
cdataImpl generator node =
    let
        unparsable message =
            Failure message node

        gen =
            generator >> Result.mapError unparsable
    in
    case node of
        Text str ->
            gen str

        Element _ _ [] ->
            -- Accepts empty tag as "empty string"
            gen ""

        Element _ _ [ Text str ] ->
            gen str

        _ ->
            Err (unparsable "The node is not a simple text node.")


{-| Similar to [`string`](#string), but also tries to convert `String` to `Int`.

    run int "<root>1</root>"
    --> Ok 1

    run int "<root>value</root>"
    --> Err "Node: <root>value</root>\ncould not convert string 'value' to an Int"

-}
int : Decoder Int
int =
    cdata (convertCdata String.toInt "an Int")


convertCdata : (String -> Maybe a) -> String -> String -> Result String a
convertCdata toType typeStr raw =
    case toType raw of
        Just a ->
            Ok a

        Nothing ->
            Err ("could not convert string '" ++ raw ++ "' to " ++ typeStr)


{-| Decodes to `Float`.

    run float "<root>1.0</root>"
    --> Ok 1.0

    run float "<root>value</root>"
    --> Err "Node: <root>value</root>\ncould not convert string 'value' to a Float"

-}
float : Decoder Float
float =
    cdata (convertCdata String.toFloat "a Float")


{-| Decodes to `Bool`.

In [Xml Schema Definition (XSD)][xsd], valid lexical representation of boolean values are,

  - 'true'
  - 'false'
  - '1'
  - '0'

We follow this specification, case-sensitively.

[xsd]: https://www.w3.org/TR/xmlschema-2/#boolean

    run bool "<root>true</root>"
    --> Ok True

    run bool "<root>value</root>"
    --> Err "Node: <root>value</root>\nNot a valid boolean value."

-}
bool : Decoder Bool
bool =
    cdata toBool


toBool : String -> Result String Bool
toBool str =
    case str of
        "true" ->
            Ok True

        "1" ->
            Ok True

        "false" ->
            Ok False

        "0" ->
            Ok False

        _ ->
            Err "Not a valid boolean value."



-- ATTRIBUTE DECODERS


{-| Decodes an attribute value of [`XmlParser.Node`][xpn] into `String`.

Fails if the node does not have specified attribute.

    run (stringAttr "attr") "<root attr='value'></root>"
    --> Ok "value"

    run (stringAttr "attr") "<root></root>"
    --> Err "Node: <root></root>\nAttribute 'attr' not found."

[xpn]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser#Node

-}
stringAttr : String -> Decoder String
stringAttr name_ =
    cdataAttr name_ Ok


cdataAttr : String -> (String -> Result String a) -> Decoder a
cdataAttr name_ generator =
    Decoder (cdataAttrImpl name_ generator)


cdataAttrImpl : String -> (String -> Result String a) -> Node -> Result Error a
cdataAttrImpl name_ generator node =
    let
        notFound =
            Failure ("Attribute '" ++ name_ ++ "' not found.") node

        gen =
            generator >> Result.mapError (\message -> Failure message node)
    in
    case node of
        Text _ ->
            Err notFound

        Element _ attrs _ ->
            attrs
                |> fetchAttributeValue name_
                |> Result.fromMaybe notFound
                |> Result.andThen gen


fetchAttributeValue : String -> List Attribute -> Maybe String
fetchAttributeValue name_ attrs =
    case attrs of
        [] ->
            Nothing

        { name, value } :: tl ->
            if name == name_ then
                Just value

            else
                fetchAttributeValue name_ tl


{-| Decodes an attribute value into `Int`.

    run (intAttr "attr") "<root attr='1'></root>"
    --> Ok 1

    run (intAttr "attr") "<root attr='value'></root>"
    --> Err "Node: <root attr=\"value\"></root>\ncould not convert string 'value' to an Int"

-}
intAttr : String -> Decoder Int
intAttr name_ =
    cdataAttr name_ (convertCdata String.toInt "an Int")


{-| Decodes an attribute value into `Float`.

    run (floatAttr "attr") "<root attr='1.5'></root>"
    --> Ok 1.5

    run (floatAttr "attr") "<root attr='value'></root>"
    --> Err "Node: <root attr=\"value\"></root>\ncould not convert string 'value' to a Float"

-}
floatAttr : String -> Decoder Float
floatAttr name_ =
    cdataAttr name_ (convertCdata String.toFloat "a Float")


{-| Decodes an attribute value into `Bool`.

    run (boolAttr "attr") "<root attr='true'></root>"
    --> Ok True

    run (boolAttr "attr") "<root attr='value'></root>"
    --> Err "Node: <root attr=\"value\"></root>\nNot a valid boolean value."

-}
boolAttr : String -> Decoder Bool
boolAttr name_ =
    cdataAttr name_ toBool



-- LIST DECODERS


{-| Composes [`ListDecoder`](#ListDecoder) that results in a singular value.

It fails if:

  - there are multiple nodes, or,
  - there are no nodes.

Examples:

    run (path [ "tag" ] (single string)) "<root><tag>string</tag></root>"
    --> Ok "string"

    run (path [ "tag" ] (single string)) "<root></root>"
    --> Err "Path: /tag\nNode: <root></root>\nNode not found."

    run (path [ "tag" ] (single string)) "<root><tag>string1</tag><tag>string2</tag></root>"
    --> Err "Path: /tag\nNode: <root><tag>string1</tag><tag>string2</tag></root>\nMultiple nodes found."

-}
single : Decoder a -> ListDecoder a
single decoder =
    ListDecoder (singleImpl decoder)


singleImpl : Decoder a -> ( List Node, Node ) -> Result Error a
singleImpl (Decoder decoder) ( nodes, ancestor ) =
    case nodes of
        [] ->
            Err (Failure "Node not found." ancestor)

        [ singleton_ ] ->
            decoder singleton_

        _ :: _ ->
            Err (Failure "Multiple nodes found." ancestor)


{-| Composes [`ListDecoder`](#ListDecoder) that results in a list of values.

This [`ListDecoder`](#ListDecoder) fails if any incoming items cannot be decoded.

    run (path [ "tag" ] (list string)) "<root><tag>string1</tag><tag>string2</tag></root>"
    --> Ok [ "string1", "string2" ]

    run (path [ "tag" ] (list int)) "<root><tag>1</tag><tag>nonInt</tag></root>"
    --> Err "Path: /tag\nNode: <tag>nonInt</tag>\ncould not convert string 'nonInt' to an Int"

-}
list : Decoder a -> ListDecoder (List a)
list decoder =
    ListDecoder (listImpl decoder [])


listImpl : Decoder a -> List a -> ( List Node, Node ) -> Result Error (List a)
listImpl (Decoder decoder) acc ( nodes, ancestor ) =
    case nodes of
        [] ->
            Ok (List.reverse acc)

        n :: ns ->
            case decoder n of
                Ok item ->
                    listImpl (Decoder decoder) (item :: acc) ( ns, ancestor )

                Err e ->
                    Err e


{-| Variation of [`list`](#list), which ignores items that cannot be decoded.

    run (path [ "tag" ] (leakyList int)) "<root><tag>1</tag><tag>nonINt</tag></root>"
    --> Ok [ 1 ]

-}
leakyList : Decoder a -> ListDecoder (List a)
leakyList (Decoder decoder) =
    ListDecoder (Tuple.first >> List.foldr (decoder >> accumlateOk) (Ok []))


accumlateOk : Result x a -> Result x (List a) -> Result x (List a)
accumlateOk result acc =
    case result of
        Err _ ->
            acc

        Ok a ->
            Result.map ((::) a) acc


{-| Similar to `Json.Decode.index`, decode from list of nodes at some index.

Useful for "narrowing down" your saerch paths.

Fails if there are not enough nodes.

    run (path [ "tag" ] (index 1 int)) "<root><tag>0</tag><tag>1</tag></root>"
    --> Ok 1

-}
index : Int -> Decoder a -> ListDecoder a
index index_ decoder =
    ListDecoder (indexImpl 0 index_ decoder)


indexImpl : Int -> Int -> Decoder a -> ( List Node, Node ) -> Result Error a
indexImpl found index_ (Decoder dec) ( nodes, ancestor ) =
    case nodes of
        [] ->
            Err <|
                Failure
                    ("Expected a node at index ["
                        ++ String.fromInt index_
                        ++ "], but only see "
                        ++ String.fromInt found
                        ++ " nodes."
                    )
                    ancestor

        node :: ns ->
            if found == index_ then
                dec node

            else
                indexImpl (found + 1) index_ (Decoder dec) ( ns, ancestor )



-- DECODER UTILITIES


{-| Decoder that always succeed with the given value.
-}
succeed : a -> Decoder a
succeed a =
    Decoder (\_ -> Ok a)


{-| Decoder that always fail with the given message.
-}
fail : String -> Decoder a
fail message =
    Decoder (failImpl message)


failImpl : String -> Node -> Result Error a
failImpl message node =
    Err (Failure message node)


{-| Try a list of decoders.

Fails if all given decoders failed, or no decoders are given.

    run (oneOf [ int, succeed 0 ]) "<root>nonInt</root>"
    --> Ok 0

    run (oneOf [ int ]) "<root>nonInt</root>"
    --> Err "All decoders failed:\n 1) Node: <root>nonInt</root>\n    could not convert string 'nonInt' to an Int"

-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder (oneOfImpl decoders [])


oneOfImpl : List (Decoder a) -> List Error -> Node -> Result Error a
oneOfImpl decoders errors node =
    case decoders of
        [] ->
            Err (OneOf (List.reverse errors))

        (Decoder d) :: ds ->
            case d node of
                Ok val ->
                    Ok val

                Err e ->
                    oneOfImpl ds (e :: errors) node


{-| Generates a decoder that depends on previous value.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen decoderBGen decoderA =
    Decoder (andThenImpl decoderBGen decoderA)


andThenImpl : (a -> Decoder b) -> Decoder a -> Node -> Result Error b
andThenImpl decoderBGen (Decoder decoderA) node =
    case decoderA node of
        Ok valA ->
            let
                (Decoder decoderB) =
                    decoderBGen valA
            in
            decoderB node

        Err e ->
            Err e


{-| Transform a decoder.

    run (map String.length string) "<root>string</root>"
    --> Ok 6

-}
map : (a -> value) -> Decoder a -> Decoder value
map valueGen decoder =
    Decoder (mapImpl valueGen decoder)


mapImpl : (a -> value) -> Decoder a -> Node -> Result Error value
mapImpl valueGen (Decoder decoder) node =
    node |> decoder |> Result.map valueGen


{-| Generates a decoder that combines results from two decoders.

It can be used for generating a decoder for a data type that takes two inputs.
Also this is used as a building block of decoder composition helpers.

    run (map2 Tuple.pair string string) "<root>string</root>"
    --> Ok ( "string", "string" )

-}
map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 valueGen decoderA decoderB =
    Decoder (map2Impl valueGen decoderA decoderB)


map2Impl : (a -> b -> value) -> Decoder a -> Decoder b -> Node -> Result Error value
map2Impl valueGen (Decoder decoderA) (Decoder decoderB) node =
    Result.map2 valueGen (decoderA node) (decoderB node)


{-| -}
map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 toVal dA dB dC =
    Decoder (map3Impl toVal dA dB dC)


map3Impl : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Node -> Result Error value
map3Impl toVal (Decoder dA) (Decoder dB) (Decoder dC) node =
    Result.map3 toVal (dA node) (dB node) (dC node)


{-| -}
map4 : (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
map4 toVal dA dB dC dD =
    Decoder (map4Impl toVal dA dB dC dD)


map4Impl : (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Node -> Result Error value
map4Impl toVal (Decoder dA) (Decoder dB) (Decoder dC) (Decoder dD) node =
    Result.map4 toVal (dA node) (dB node) (dC node) (dD node)


{-| -}
map5 :
    (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder value
map5 toVal dA dB dC dD dE =
    Decoder (map5Impl toVal dA dB dC dD dE)


map5Impl :
    (a -> b -> c -> d -> e -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Node
    -> Result Error value
map5Impl toVal (Decoder dA) (Decoder dB) (Decoder dC) (Decoder dD) (Decoder dE) node =
    Result.map5 toVal (dA node) (dB node) (dC node) (dD node) (dE node)


{-| Equivalent to [`Json.Decode.Extra.andMap`][jdeam], allows writing XML decoders in sequential style.

[jdeam]: http://package.elm-lang.org/packages/elm-community/json-extra/latest/Json-Decode-Extra#andMap

    run
        (succeed Tuple.pair
            |> andMap (path ["string"] (single string))
            |> andMap (path ["int"] (single int))
        )
        "<root><string>string</string><int>1</int></root>"
    --> Ok ("string", 1)

-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


{-| Generates a decoder that results in the default value on failure.

    run (withDefault 0 int) "<root>1</root>"
    --> Ok 1

    run (withDefault 0 int) "<root>Non Int value</root>"
    --> Ok 0

-}
withDefault : a -> Decoder a -> Decoder a
withDefault default decoder =
    oneOf
        [ decoder
        , succeed default
        ]


{-| Generates a decoder that results in a `Maybe` value.

If the given decoder resulted in `Err`, it succeeds with `Nothing`.
Otherwise (in cases of `Ok`,) it succeeds with `Just` value.

    run (maybe int) "<root>1</root>"
    --> Ok (Just 1)

    run (maybe int) "<root>Non Int value</root>"
    --> Ok Nothing

-}
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    oneOf
        [ map Just decoder
        , succeed Nothing
        ]


{-| Generates a lazy decoder.

Similar to [`Json.Decode.lazy`][jdl].

    someRecordDecoder : Decoder SomeRecord
    someRecordDecoder =
        map2 SomeRecord
            (path [ "path", "to", "string", "value" ] (single string))
            (path [ "path", "to", "list", "of", "someRecord" ] (list (lazy (\_ -> someRecordDecoder))))

With this, you can avoid "bad-recursion" error on compilation
which happens when you define nested part of the above decoder as `(list someRecordDecoder)`

[jdl]: http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode#lazy

-}
lazy : (() -> Decoder a) -> Decoder a
lazy =
    \a -> andThen a (succeed ())



-- NODE LOCATER


{-| Generates a [`Decoder`](#Decoder) that applies a [`ListDecoder`](#ListDecoder) at specified XML path.

Typical usage:

    someRecordDecoder : Decoder SomeRecord
    someRecordDecoder =
        map2 SomeRecord
            (path [ "string", "value" ] (single string))
            (path [ "int", "values" ] (list int))

Due to the nature of XML, you cannot distinguish a particular tag or tags hold
whether "singular value" or "list of values", from the structure of XML document itself.
This is opposite to JSON, where there can be only one field of a paticular key in a level,
and its quantization is obvious from the shape of the value.

For this reason, this function always produce list of [`XmlParser.Node`][xpn]s.
Then they must be decoded using special decoder parts: [`ListDecoder`](#ListDecoder).
In the above example, [`single`](#single) and [`list`](#list) are [`ListDecoder`](#ListDecoder) generators.

[xpn]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser#Node

It collects ALL nodes matching the path in breadth-first manner.
If you need to collect nodes only from a specific intermediate node that fulfills some conditions,
(e.g. `path [ "a", "b" ]`, but only in 3rd "<a>...</a>")
you need to split the path and explicitly define a Decoder that narrows down the path.
Be careful not to accidentally include nodes from not-targeted ancestors.

Note that in the path, you must "start" at the root scope.
For instance, to work with an XML document like:

<pre>
&lt;Root&gt;
    &lt;Path&gt;
        &lt;Target&gt;
            Value
        &lt;/Target&gt;
    &lt;/Path&gt;
&lt;/Root&gt;
</pre>

You should specify:

    path [ "Path", "Target" ] (single string)

Basic usages:

    run (path [ "tag", "nested" ] (single string)) "<root><tag><nested>string1</nested></tag></root>"
    --> Ok "string1"

    run (path [ "tag", "nested" ] (list string)) "<root><tag><nested>string1</nested><nested>string2</nested></tag></root>"
    --> Ok [ "string1", "string2" ]

    run (path [ "tag", "nested" ] (single (stringAttr "value"))) "<root><tag><nested value='attr1'></nested></tag></root>"
    --> Ok "attr1"

Decoders will report errors with path at which error happened as well as nearest node:

    run (path [ "tag", "nested" ] (single int)) "<root><tag><nested>string1</nested></tag></root>"
    --> Err "Path: /tag/nested\nNode: <nested>string1</nested>\ncould not convert string 'string1' to an Int"

-}
path : List String -> ListDecoder a -> Decoder a
path path_ listDecoder =
    Decoder (pathImpl path_ listDecoder)


pathImpl : List String -> ListDecoder a -> Node -> Result Error a
pathImpl path_ (ListDecoder listDecoder) node =
    node
        |> children
        |> query path_ node
        |> listDecoder
        |> Result.mapError (concatPath path_)


children : Node -> List Node
children node =
    case node of
        Element _ _ nodes ->
            nodes

        Text _ ->
            []


query : List String -> Node -> List Node -> ( List Node, Node )
query path_ ancestor collected =
    case path_ of
        [] ->
            ( collected, ancestor )

        [ segment ] ->
            -- This clause is necessary in order to keep "empty" node as is, like "<val></val>".
            -- Without special care, such nodes are silently eliminated by `List.concatMap children`.
            ( List.filter (hasName segment) collected, ancestor )

        segment :: ss ->
            case List.filter (hasName segment) collected of
                [] ->
                    -- It is pointless to dig any deeper
                    ( [], ancestor )

                [ onlyOne ] ->
                    -- This is the only node that produced path-matching element,
                    -- effectively "narrowing down" pathfinding context for more consice error messages
                    query ss onlyOne (children onlyOne)

                many ->
                    query ss ancestor (List.concatMap children many)


hasName : String -> Node -> Bool
hasName name node =
    case node of
        Element nodeName _ _ ->
            name == nodeName

        Text _ ->
            False


concatPath : List String -> Error -> Error
concatPath path_ error =
    case error of
        Path innerPath innerError ->
            Path (path_ ++ innerPath) innerError

        otherwise ->
            Path path_ otherwise



-- PIPELINE APIS


{-| Decodes value at required XML path.

    pipelineDecoder : Decoder ( String, List Int )
    pipelineDecoder =
        succeed Tuple.pair
            |> requiredPath [ "path", "to", "string", "value" ] (single string)
            |> requiredPath [ "path", "to", "int", "values" ] (list int)

    run pipelineDecoder
        """
        <root>
            <path>
                <to>
                    <string>
                        <value>SomeString</value>
                    </string>
                    <int>
                        <values>1</values>
                        <values>2</values>
                    </int>
                </to>
            </path>
        </root>
        """
    --> Ok ( "SomeString", [ 1, 2 ] )

-}
requiredPath : List String -> ListDecoder a -> Decoder (a -> b) -> Decoder b
requiredPath path_ listDecoderA =
    map2 (|>) (path path_ listDecoderA)


{-| Tries to decode value at optional XML path. Uses default value if the node is missing.

    decoderWithDefault : Decoder String
    decoderWithDefault =
        succeed identity
            |> optionalPath [ "optional", "path" ] (single string) "default"

    run decoderWithDefault "<root><optional><path>string</path></optional></root>"
    --> Ok "string"

    run decoderWithDefault "<root></root>"
    --> Ok "default"

-}
optionalPath : List String -> ListDecoder a -> a -> Decoder (a -> b) -> Decoder b
optionalPath path_ listDecoderA default =
    map2 (|>) (withDefault default (path path_ listDecoderA))


{-| Decodes value at possible XML path into `Maybe` value.

    maybeDecoder : Decoder (Maybe String)
    maybeDecoder =
        succeed identity
            |> possiblePath [ "possible", "path" ] (single string)

    run maybeDecoder "<root><possible><path>string</path></possible></root>"
    --> Ok (Just "string")

    run maybeDecoder "<root></root>"
    --> Ok Nothing

If you want to apply default value when the node is missing, use [`optionalWith`](#optionalWith).

-}
possiblePath : List String -> ListDecoder a -> Decoder (Maybe a -> b) -> Decoder b
possiblePath path_ listDecoderA =
    map2 (|>) (maybe (path path_ listDecoderA))



-- ERROR UTILITY


{-| Convert [`Error`](#Error) to a formatted string.
-}
errorToString : Error -> String
errorToString error =
    errorToRows error |> String.join "\n"


errorToRows : Error -> List String
errorToRows error =
    case error of
        Path path_ innerError ->
            ("Path: /" ++ String.join "/" path_) :: errorToRows innerError

        OneOf [] ->
            [ "No decoders available." ]

        OneOf innerErrors ->
            let
                innerRows =
                    innerErrors |> List.indexedMap genChildRows |> List.concat

                genChildRows outerIndex =
                    errorToRows >> List.indexedMap (indentRow (outerIndex + 1))

                indentRow outerIndex innerIndex row =
                    if innerIndex == 0 then
                        String.padLeft 2 ' ' (String.fromInt outerIndex) ++ ") " ++ row

                    else
                        "    " ++ row
            in
            "All decoders failed:" :: innerRows

        Failure problem node ->
            [ "Node: " ++ formatNode node
            , problem
            ]
