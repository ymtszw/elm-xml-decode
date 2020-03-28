module Xml.Decode.Internal exposing (escape, formatNode)

{-| Internal functions.
-}

import XmlParser exposing (Attribute, Node(..))


{-| Format XML node for error dump. Always produce end tags.
-}
formatNode : Node -> String
formatNode node =
    case node of
        Element tagName attrs children ->
            "<"
                ++ escape tagName
                ++ attributesToString attrs
                ++ ">"
                ++ (children |> List.map formatNode |> String.join "")
                ++ "</"
                ++ escape tagName
                ++ ">"

        Text s ->
            escape s


attributesToString : List Attribute -> String
attributesToString attrs =
    case attrs of
        [] ->
            ""

        _ ->
            " " ++ (attrs |> List.map formatAttribute |> String.join " ")


formatAttribute : Attribute -> String
formatAttribute attribute =
    escape attribute.name ++ "=\"" ++ escape attribute.value ++ "\""


{-| Replaces by char-based predicate.
-}
escape : String -> String
escape s =
    let
        reducer char =
            case char of
                '&' ->
                    String.append "&amp;"

                '<' ->
                    String.append "&lt;"

                '>' ->
                    String.append "&gt;"

                '"' ->
                    String.append "&quot;"

                '\'' ->
                    String.append "&apos;"

                c ->
                    String.cons c
    in
    String.foldr reducer "" s
