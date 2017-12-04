module Xml.Decode.Pipeline exposing (requiredPath, possiblePath, optionalPath)

{-| Xml decoder module sharing the spirit of [`Json.Decode.Pipeline`][jdp].

[jdp]: http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/Json-Decode-Pipeline

Leverages basic `(|>)` operator by [`requiredPath`](#requiredPath), [`optionalPath`](#optionalPath) and [`possiblePath`](#possiblePath),
allowing DSL style decoder composition.

    import Xml.Decode exposing (succeed, singleton, string)

    someRecordDecoder : Decoder SomeRecord
    someRecordDecoder =
        succeed SomeRecord
            |> requiredPath [ "path", "to", "textField1" ] (singleton string)
            |> requiredPath [ "path", "to", "textField2" ] (singleton string)

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
-}
optionalPath : List String -> ListDecoder a -> a -> Decoder (a -> b) -> Decoder b
optionalPath path_ listDecoderA default =
    map2 (|>) (Xml.Decode.withDefault default (path path_ listDecoderA))


{-| Decodes value at possible XML path into `Maybe` value.

If you want to apply default value when the node is missing, use [`optionalWith`](#optionalWith).

-}
possiblePath : List String -> ListDecoder a -> Decoder (Maybe a -> b) -> Decoder b
possiblePath path_ listDecoderA =
    map2 (|>) (Xml.Decode.maybe (path path_ listDecoderA))
