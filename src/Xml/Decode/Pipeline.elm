module Xml.Decode.Pipeline exposing (requiredPath, optionalPath, possiblePath)

{-| Xml decoder module sharing the spirit of [`Json.Decode.Pipeline`][jdp].

[jdp]: http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/Json-Decode-Pipeline

Leverages basic `(|>)` operator by [`requiredPath`](#requiredPath),
[`optionalPath`](#optionalPath) and [`possiblePath`](#possiblePath),
allowing DSL style decoder composition.

    import Xml.Decode exposing (Decoder, succeed, single, list, string, int)

    pipelineDecoder : Decoder ( String, List Int )
    pipelineDecoder =
        succeed (,)
            |> requiredPath [ "path", "to", "string", "value" ] (single string)
            |> requiredPath [ "path", "to", "int", "values" ] (list int)

    Xml.Decode.run pipelineDecoder
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

Benefit of this style is it uses `Basic.|>`
so you do not have to import non-standard infix operator.


# Pipeline Node Locaters

@docs requiredPath, optionalPath, possiblePath

-}

import Xml.Decode exposing (Decoder, ListDecoder, map2, path)


{-| Decodes value at required XML path.
-}
requiredPath : List String -> ListDecoder a -> Decoder (a -> b) -> Decoder b
requiredPath path_ listDecoderA =
    map2 (|>) (path path_ listDecoderA)


{-| Tries to decode value at optional XML path. Uses default value if the node is missing.

    import Xml.Decode exposing (Decoder, succeed, single, string)

    decoderWithDefault : Decoder String
    decoderWithDefault =
        succeed identity
            |> optionalPath [ "optional", "path" ] (single string) "default"

    Xml.Decode.run decoderWithDefault "<root><optional><path>string</path></optional></root>"
    --> Ok "string"

    Xml.Decode.run decoderWithDefault "<root></root>"
    --> Ok "default"

-}
optionalPath : List String -> ListDecoder a -> a -> Decoder (a -> b) -> Decoder b
optionalPath path_ listDecoderA default =
    map2 (|>) (Xml.Decode.withDefault default (path path_ listDecoderA))


{-| Decodes value at possible XML path into `Maybe` value.

    import Xml.Decode exposing (Decoder, succeed, single, string)

    maybeDecoder : Decoder (Maybe String)
    maybeDecoder =
        succeed identity
            |> possiblePath [ "possible", "path" ] (single string)

    Xml.Decode.run maybeDecoder "<root><possible><path>string</path></possible></root>"
    --> Ok (Just "string")

    Xml.Decode.run maybeDecoder "<root></root>"
    --> Ok Nothing

If you want to apply default value when the node is missing, use [`optionalWith`](#optionalWith).

-}
possiblePath : List String -> ListDecoder a -> Decoder (Maybe a -> b) -> Decoder b
possiblePath path_ listDecoderA =
    map2 (|>) (Xml.Decode.maybe (path path_ listDecoderA))
