# Changelog

## Upcoming

- Now uses [miniBill/elm-xml-parser](https://package.elm-lang.org/packages/miniBill/elm-xml-parser/latest/) fork for parsing XML. Thanks [@miniBill](https://github.com/miniBill)!

## 3.2.0

- Added `node` for customized decoding. Thanks [@marmutro](https://github.com/marmutro)!

## 3.1.0

- Added `with` for CPS.
- [Misc change] Upgraded benchmark.

## 3.0.0

- Added `oneOf`, `index`.
- Re-defined `Error` structure to fit `OneOf`. Also it is aligned with `Json.Decode.Error` now.
- [Behavior change] Improved error messages. Now they have newlines with indented structure.

## 2.0.0

- `Decoder` and `ListDecoder` are now opaque types.
- `fail` now takes plain `String`, allowing easier failure construction.
- Removed `date` and `dateAttr` since core `Date` module has been removed.
  - For now, pipe `string` or `stringAttr` into `andThen` with your favorite date string parser,
    such as [rtfeldman/elm-iso8601-date-strings][iso].
- Removed `(|:)` since custom infix operators are no longer supported.
  - You may replace them with `|> andMap ( ... )`, or use Pipeline-style.
- Collapse `Xml.Decode.Extra` and `Xml.Decode.Pipeline` into `Xml.Decode`.
- [Behavior change] Elements with no children are now decoded into empty strings (`""`) when using `string`.

[iso]: https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/Iso8601

## 1.0.0

- Initial release 🎉
