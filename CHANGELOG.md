# Next major release

* Elements with no children are now decoded into empty strings (`""`) when using `string`.
* Removed `date` and `dateAttr` since core `Date` module has been removed.
    * For now, pipe `string` or `stringAttr` into `andThen` with your favorite date string parser,
      such as [rtfeldman/elm-iso8601-date-strings][iso].
* Removed `(|:)` since custom infix operators are no longer supported.
    * You may replace them with `|> andMap ( ... )`, or use Pipeline-style.
* Collapse `Xml.Decode.Extra` and `Xml.Decode.Pipeline` into `Xml.Decode`.

[iso]: https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/Iso8601
