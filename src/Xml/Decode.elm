module Xml.Decode
    exposing
        ( Decoder
        , ListDecoder
        , Error(SimpleError, DetailedError)
        , Problem(NodeNotFound, AttributeNotFound, Duplicate, Unparsable)
        , run
        , decodeString
        , decodeXml
        , string
        , int
        , float
        , bool
        , date
        , stringAttr
        , intAttr
        , floatAttr
        , boolAttr
        , dateAttr
        , single
        , list
        , leakyList
        , succeed
        , fail
        , andThen
        , map
        , map2
        , withDefault
        , maybe
        , lazy
        , path
        , errorToString
        )

{-| XML decoder module sharing the spirit of [`Json.Decode`][jd].

Using [`XmlParser`][xp] as its parser component.

[jd]: http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode
[xp]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser


# Basic Example

Also see examples in [`Xml.Decode.Pipeline`](./Xml-Decode-Pipeline)
and [`Xml.Decode.Extra`](./Xml-Decode-Extra) for alternative styles.

Examples in this package are doc-tested.

    exampleDecoder : Decoder ( String, List Int )
    exampleDecoder =
        map2 (,)
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

@docs Decoder, ListDecoder, Error, Problem


# Decode Executor

@docs run, decodeString, decodeXml


# Decoders

@docs string, int, float, bool, date


# Attribute Decoders

@docs stringAttr, intAttr, floatAttr, boolAttr, dateAttr


# List Decoders

@docs single, list, leakyList


# Decoder Utilities

@docs succeed, fail, andThen, map, map2, withDefault, maybe, lazy


# Node Locater

@docs path


# Error Utility

@docs errorToString

-}

import Date exposing (Date)
import XmlParser exposing (Xml, Node(Text, Element), Attribute)


-- TYPES


{-| A function that knows how to decode an XML node into Elm value.
-}
type alias Decoder a =
    Node -> Result Error a


{-| A function that knows how to decode list of XML nodes into Elm value.

Used in conjunction with "query" functions such as [`path`](#path).

-}
type alias ListDecoder a =
    List Node -> Result Error a


{-| Represents error on decode execution.

`DetailedError` will tell you:

  - where the error happened
  - what kind of node actually was there

-}
type Error
    = SimpleError Problem
    | DetailedError (List String) Node Problem


{-| Possible problems that can happen during decoding.
-}
type Problem
    = NodeNotFound
    | AttributeNotFound String
    | Duplicate
    | Unparsable String



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
decodeString decoder =
    let
        decodeAndMapError =
            decodeXml decoder >> Result.mapError errorToString
    in
        -- Could use disassembling Parser.Error into more readable string here; though we skip it for now
        XmlParser.parse >> Result.mapError toString >> Result.andThen decodeAndMapError


{-| Decodes an [`XmlParser.Xml`][xpx] value into other type of Elm value.

[xpx]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser#Xml

It discards Document Type Definitoin (DTD) and Processing Instruction in XML,
only cares about root XML node.

    import XmlParser exposing (Xml, Node(..))

    exampleDecoder : Decoder ( String, List Int )
    exampleDecoder =
        map2 (,)
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
decodeXml decoder { root } =
    decoder root



-- DECODERS


{-| Decodes an [`XmlParser.Node`][xpn] into `String`.

  - If the node is `XmlParser.Text`, extracts its value.
  - If the node is `XmlParser.Element` AND contains a single `XmlParser.Text` child,
    extracts its value.
  - Otherwise fails.

If you want to extract values from node attribute, use [`stringAttr`](#stringAttr) and variants.

[xpn]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser#Node

    run string "<root>string</root>"
    --> Ok "string"

    run string "<root><nested>string</nested></root>"
    --> Err "The node is not a simple text node. At: /, Node: Element \"root\" [] ([Element \"nested\" [] ([Text \"string\"])])"

-}
string : Decoder String
string node =
    case node of
        Text str ->
            Ok str

        Element _ _ [ Text str ] ->
            Ok str

        _ ->
            Err <| DetailedError [] node (Unparsable "The node is not a simple text node.")


{-| Similar to [`string`](#string), but also tries to convert `String` to `Int`.

    run int "<root>1</root>"
    --> Ok 1

    run int "<root>value</root>"
    --> Err "could not convert string 'value' to an Int"

-}
int : Decoder Int
int =
    string >> Result.andThen (String.toInt >> mapParseError)


mapParseError : Result String a -> Result Error a
mapParseError =
    Result.mapError (SimpleError << Unparsable)


{-| Decodes to `Float`.

    run float "<root>1.0</root>"
    --> Ok 1.0

    run float "<root>value</root>"
    --> Err "could not convert string 'value' to a Float"

-}
float : Decoder Float
float =
    string >> Result.andThen (String.toFloat >> mapParseError)


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
    --> Err "Not a valid boolean value."

-}
bool : Decoder Bool
bool =
    string >> Result.andThen (toBool >> mapParseError)


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


{-| Decodes to `Date`.

[It uses `new Date()` of JavaScript under the hood][date].

[Likely this needs to be updated for later versoins of core][depr].

[date]: https://github.com/elm-lang/core/blob/5.1.1/src/Native/Date.js#L5
[depr]: https://github.com/elm-lang/core/commit/a892fdf705f83523752c5469384e9880fbdfe3b1#diff-25d902c24283ab8cfbac54dfa101ad31

-}
date : Decoder Date
date =
    string >> Result.andThen (Date.fromString >> mapParseError)



-- ATTRIBUTE DECODERS


{-| Decodes an attribute value of [`XmlParser.Node`][xpn] into `String`.

Fails if the node does not have specified attribute.

    run (stringAttr "attr") "<root attr='value'></root>"
    --> Ok "value"

    run (stringAttr "attr") "<root></root>"
    --> Err "Attribute 'attr' not found. At: /, Node: Element \"root\" [] []"

[xpn]: http://package.elm-lang.org/packages/jinjor/elm-xml-parser/latest/XmlParser#Node

-}
stringAttr : String -> Decoder String
stringAttr name_ node =
    let
        error =
            DetailedError [] node (AttributeNotFound name_)
    in
        case node of
            Text _ ->
                Err error

            Element _ attrs _ ->
                attrs
                    |> fetchAttributeValue name_
                    |> Result.fromMaybe error


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
    --> Err "could not convert string 'value' to an Int"

-}
intAttr : String -> Decoder Int
intAttr name_ =
    stringAttr name_ >> Result.andThen (String.toInt >> mapParseError)


{-| Decodes an attribute value into `Float`.

    run (floatAttr "attr") "<root attr='1.5'></root>"
    --> Ok 1.5

    run (floatAttr "attr") "<root attr='value'></root>"
    --> Err "could not convert string 'value' to a Float"

-}
floatAttr : String -> Decoder Float
floatAttr name_ =
    stringAttr name_ >> Result.andThen (String.toFloat >> mapParseError)


{-| Decodes an attribute value into `Bool`.

    run (boolAttr "attr") "<root attr='true'></root>"
    --> Ok True

    run (boolAttr "attr") "<root attr='value'></root>"
    --> Err "Not a valid boolean value."

-}
boolAttr : String -> Decoder Bool
boolAttr name_ =
    stringAttr name_ >> Result.andThen (toBool >> mapParseError)


{-| Decodes an attribute value into `Date`.
-}
dateAttr : String -> Decoder Date
dateAttr name_ =
    stringAttr name_ >> Result.andThen (Date.fromString >> mapParseError)



-- LIST DECODERS


{-| Composes [`ListDecoder`](#ListDecoder) that results in a singular value.

It fails if:

  - there are multiple nodes, or,
  - there are no nodes.

Examples:

    run (path [ "tag" ] (single string)) "<root><tag>string</tag></root>"
    --> Ok "string"

    run (path [ "tag" ] (single string)) "<root></root>"
    --> Err "Node not found. At: /tag, Node: Element \"root\" [] []"

    run (path [ "tag" ] (single string)) "<root><tag>string1</tag><tag>string2</tag></root>"
    --> Err "Multiple nodes found. At: /tag, Node: Element \"root\" [] ([Element \"tag\" [] ([Text \"string1\"]),Element \"tag\" [] ([Text \"string2\"])])"

-}
single : Decoder a -> ListDecoder a
single decoder nodes =
    case nodes of
        [] ->
            Err <| SimpleError NodeNotFound

        [ singleton_ ] ->
            decoder singleton_

        _ :: _ ->
            Err <| SimpleError Duplicate


{-| Composes [`ListDecoder`](#ListDecoder) that results in a list of values.

This [`ListDecoder`](#ListDecoder) fails if any incoming items cannot be decoded.

    run (path [ "tag" ] (list string)) "<root><tag>string1</tag><tag>string2</tag></root>"
    --> Ok [ "string1", "string2" ]

    run (path [ "tag" ] (list string)) "<root><tag>string1</tag><tag><nested>string2</nested></tag></root>"
    --> Err "The node is not a simple text node. At: /tag, Node: Element \"tag\" [] ([Element \"nested\" [] ([Text \"string2\"])])"

-}
list : Decoder a -> ListDecoder (List a)
list decoder =
    List.foldr (listReducer decoder) (Ok [])


listReducer : Decoder a -> Node -> Result Error (List a) -> Result Error (List a)
listReducer decoder node accResult =
    node
        |> decoder
        |> Result.map2 (flip (::)) accResult
        |> Result.mapError (addPathAndNode [] node)


{-| Variation of [`list`](#list), which ignores items that cannot be decoded.

    run (path [ "tag" ] (leakyList string)) "<root><tag>string1</tag><tag><nested>string2</nested></tag></root>"
    --> Ok [ "string1" ]

-}
leakyList : Decoder a -> ListDecoder (List a)
leakyList decoder =
    List.foldr (decoder >> accumlateOk) (Ok [])


accumlateOk : Result x a -> Result x (List a) -> Result x (List a)
accumlateOk result acc =
    case result of
        Err _ ->
            acc

        Ok a ->
            Result.map ((::) a) acc



-- DECODER UTILITIES


{-| Decoder that always succeed with the given value.
-}
succeed : a -> Decoder a
succeed a =
    always (Ok a)


{-| Decoder that always fail with the given message.
-}
fail : Error -> Decoder a
fail error =
    always (Err error)


{-| Generates a decoder that depends on previous value.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen decoderBGen decoderA node =
    case decoderA node of
        Ok valA ->
            node |> decoderBGen valA

        Err e ->
            Err e


{-| Transform a decoder.

    run (map String.length string) "<root>string</root>"
    --> Ok 6

-}
map : (a -> value) -> Decoder a -> Decoder value
map valueGen decoder =
    decoder >> Result.map valueGen


{-| Generates a decoder that combines results from two decoders.

It can be used for generating a decoder for a data type that takes two inputs.
Although mainly, this is used as a building block of DSL style decoder generation.

Also see `Xml.Decode.Pipeline` or `Xml.Decode.Extra`.

    run (map2 (,) string string) "<root>string</root>"
    --> Ok ( "string", "string" )

-}
map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 valueGen decoderA decoderB node =
    Result.map2 valueGen
        (decoderA node)
        (decoderB node)


{-| Generates a decoder that results in the default value on failure.

    run (withDefault 0 int) "<root>1</root>"
    --> Ok 1

    run (withDefault 0 int) "<root>Non Int value</root>"
    --> Ok 0

-}
withDefault : a -> Decoder a -> Decoder a
withDefault default decoder =
    let
        applyDefault result =
            case result of
                Ok _ ->
                    result

                Err _ ->
                    Ok default
    in
        decoder >> applyDefault


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
    let
        maybify result =
            case result of
                Ok val ->
                    Ok (Just val)

                Err _ ->
                    Ok Nothing
    in
        decoder >> maybify


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
    flip andThen (succeed ())



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

If you want to perform complex resolution of multiple matches from `path`,
you can implement your own [`ListDecoder`](#ListDecoder)s.

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

    path ["Path", "Target"] (single string)

-}
path : List String -> ListDecoder a -> Decoder a
path path_ listDecoder node =
    node |> children |> query path_ |> decodeWithErrorContext path_ node listDecoder


children : Node -> List Node
children node =
    case node of
        Element _ _ nodes ->
            nodes

        Text _ ->
            []


query : List String -> List Node -> List Node
query path_ nodes =
    case path_ of
        [] ->
            nodes

        [ k ] ->
            nodes
                |> List.filter (hasName k)

        k :: ks ->
            nodes
                |> List.filter (hasName k)
                |> List.concatMap children
                |> query ks


hasName : String -> Node -> Bool
hasName name node =
    case node of
        Element nodeName _ _ ->
            name == nodeName

        Text _ ->
            False


decodeWithErrorContext : List String -> Node -> ListDecoder a -> List Node -> Result Error a
decodeWithErrorContext path_ node listDecoder nodes =
    case listDecoder nodes of
        Ok ok ->
            Ok ok

        Err err ->
            Err <| addPathAndNode path_ node err



-- ERROR UTILITY


addPathAndNode : List String -> Node -> Error -> Error
addPathAndNode path_ node error =
    case error of
        SimpleError r ->
            DetailedError path_ node r

        DetailedError innerPath innerNode r ->
            DetailedError (path_ ++ innerPath) innerNode r


{-| Convert [`Error`](#Error) to a formatted string.
-}
errorToString : Error -> String
errorToString error =
    case error of
        SimpleError r ->
            problemToString r

        DetailedError path_ node r ->
            problemToString r
                ++ (" At: /" ++ String.join "/" path_)
                -- We would like to have formatNode here...
                ++ (", Node: " ++ toString node)


problemToString : Problem -> String
problemToString reason =
    case reason of
        NodeNotFound ->
            "Node not found."

        AttributeNotFound name ->
            "Attribute '" ++ name ++ "' not found."

        Duplicate ->
            "Multiple nodes found."

        Unparsable str ->
            str
