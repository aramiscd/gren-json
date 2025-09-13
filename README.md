# gren-json

Note that Gren's standard library is already equipped with modules for
[encoding](https://packages.gren-lang.org/package/gren-lang/core/version/latest/module/Json.Encode) and
[decoding](https://packages.gren-lang.org/package/gren-lang/core/version/latest/module/Json.Decode) JSON documents.

---

This package contains

- a JSON data type,
- a JSON parser,
- a function for pretty printing JSON documents.

The parser is based on [aramiscd/gren-parse](https://packages.gren-lang.org/package/aramiscd/gren-parse).
It is not very efficient but it strictly follows the grammar on [json.org](https://www.json.org/json-en.html),
so I'm pretty sure it can consume any valid JSON as long as it doesn't choke on the amount of data.

This package is still useful if

- you want to explore a very simple combinaty parser,
- if you need a simple intermediate data type for JSON values, or
- if you like your JSON formatted independently of whitespace and with prefix commas.

Package documentation:
[packages.gren-lang.org/package/aramiscd/gren-json](https://packages.gren-lang.org/package/aramiscd/gren-json)

See [aramiscd/jsonfmt](https://github.com/aramiscd/jsonfmt) for a ready-to-use JSON formatter based on this library.
