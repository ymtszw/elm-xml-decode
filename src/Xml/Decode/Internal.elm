module Xml.Decode.Internal exposing (escape, escapeR, formatNode)

{-| Internal functions.
-}

import Regex
import XmlParser exposing (Attribute, Node(..), Xml)


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


{-| Replaces by regex for comparison.
-}
escapeR : String -> String
escapeR s =
    s
        |> Regex.replace Regex.All (Regex.regex "&") (\_ -> "&amp;")
        |> Regex.replace Regex.All (Regex.regex "<") (\_ -> "&lt;")
        |> Regex.replace Regex.All (Regex.regex ">") (\_ -> "&gt;")
        |> Regex.replace Regex.All (Regex.regex "\"") (\_ -> "&quot;")
        |> Regex.replace Regex.All (Regex.regex "'") (\_ -> "&apos;")
