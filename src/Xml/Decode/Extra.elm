module Xml.Decode.Extra exposing (andMap, (|:))

{-| Xml decoder module sharing the spirit of [`Json.Decode.Extra`][jde].

[jde]: http://package.elm-lang.org/packages/elm-community/json-extra/latest/Json-Decode-Extra

Uses [`(|:)`](#|:) infix operator that allows writing decoders in sequential style like so:

    import Xml.Decode exposing (succeed, path, singleton, string)

    someRecordDecoder : Decoder SomeRecord
    someRecordDecoder =
        succeed SomeRecord
            |: path [ "path", "to", "textField1" ] (singleton string)
            |: path [ "path", "to", "textField2" ] (singleton string)

Benefit of this style is it leverages standard decoders from [`Xml.Decode`](./Xml-Decode),
so you only have to import [`(|:)`](#|:).

Also, this style can be considered "Applicative" style of sequential application.
`(|:)` is essentially [Applicative's `(<*>)`][ap].

[ap]: http://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Applicative.html#v:-60--42--62-


# Sequential Decoding

@docs andMap, (|:)

-}

import Xml.Decode exposing (Decoder)


{-| Equivalent to [`Json.Decode.Extra.andMap`][jdeam], allows writing XML decoders in sequential style.

[jdeam]: http://package.elm-lang.org/packages/elm-community/json-extra/latest/Json-Decode-Extra#andMap

-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Xml.Decode.map2 (|>)


{-| Equivalent to [`Json.Decode.Extra.(|:)`][jde|:], infix version of [`andMap`](#andMap).

[jde|:]: http://package.elm-lang.org/packages/elm-community/json-extra/latest/Json-Decode-Extra#|:

-}
(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) =
    flip andMap
