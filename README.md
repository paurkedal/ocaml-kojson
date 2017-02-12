# Kojson - JSON Parser Combinators

## Synopsis

Kojson provides combinators to construct functions to match JSON values in a
convenient and asymptotically efficient way.  The patterns take as input the
polymorphic variant `Yojson.Basic.json` wrapped with location information
for precise error reporting.  The library provides two top-level modules:

  * `Kojson` - Contains general definitions.
  * `Kojson_pattern` - Contains the combinators. You should open this where
    needed.  It provides a `K` substructure to match trees, a `Ka`
    substructure to match associations, and some operators.

For details, see [the API reference](http://paurkedal.github.io/ocaml-kojson/).

### Example

The following decodes a list of objects containing two mandatory and one
optional attributes:

```OCaml
open Kojson_pattern
open Unprime
open Unprime_option

let decode_log =
  K.list begin
    K.assoc begin
      "time"^: K.int *> fun time ->
      "facility"^?: Option.map K.string *> fun facility_opt ->
      "message"^: K.string *> fun message ->
      Ka.stop (time, facility_opt, message)
    end
  end
```

The input to this is constructed with `Kojson.jin_of_json` and it will throw
a `Kojson.Mismatch` exception if the pattern does not match.  E.g.  loading
the JSON data from a file:

```OCaml
let handle_error fp msg = failwith ("Cannot load "^fp^": "^msg^"\n")

let load_log_file fp =
  try decode_log (Kojson.jin_of_json (Yojson.Basic.from_file fp)) with
  | Yojson.Json_error msg -> handle_error fp msg
  | Kojson.Mismatch (path, expect) ->
    handle_error fp (Kojson.string_of_mismatch (path, expect))
```

If the file does not contain valid JSON data, then Yojson will point to the
file and line number.  If the pattern does not match, then Kojson will
indicate the logical path of the mismatch using labels and indices.

## Installation

The package is available though the [author's OPAM repository][1]:

    opam repo add paurkedal https://github.com/paurkedal/opam-repo-paurkedal.git
    opam install kojson

Alternatively use `topkg/pkg.ml` (or `opam pin`) from a Git checkout or tar
extraction.

### Dependencies

* [Prime](https://github.com/paurkedal/ocaml-prime)
* [Topkg](http://erratique.ch/software/topkg) (build only)
* [Yojson](http://mjambon.com/yojson.html)

### See Also

* [atdgen](https://github.com/mjambon/atdgen)
* [deriving-yojson](https://github.com/hhugo/deriving-yojson)
* [jsont](http://erratique.ch/software/jsont)
* [piqi](http://piqi.org/)
* [ppx_deriving_yojson](https://github.com/whitequark/ppx_deriving_yojson)

[1]: https://github.com/paurkedal/opam-repo-paurkedal
