(* Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf
open Unprime_list

type json = Yojson.Basic.t

type path = Kojson_private.path

let string_of_pathcomp = function
  | `Label l -> l
  | `Index i -> sprintf "#%d" i

let string_of_path = function
  | [] -> "root"
  | xs -> String.concat "." (List.map string_of_pathcomp xs)

type expectation =
  [ `expecting_value of json
  | `expecting_values of json list
  | `expecting_type of string
  | `expecting_label of string
  | `not_expecting_labels of string list
  | `either of (path * expectation) list ]

exception Mismatch of path * expectation

let rec string_of_expectation = function
  | `expecting_value v ->
    sprintf "Expecting the value %s" (Yojson.Basic.to_string v)
  | `expecting_values vs ->
    sprintf "Expecting any of %s"
      (String.concat ", " (List.map Yojson.Basic.to_string vs))
  | `expecting_type t ->
    sprintf "Expecting a value of type %s" t
  | `expecting_label l ->
    sprintf "Missing mandatory label %s" l
  | `not_expecting_labels ls ->
    sprintf "Unexpected labels {%s}" (String.concat ", " ls)
  | `either cases ->
    "Either " ^
      (String.concat " or "
        (List.map (String.uncapitalize_ascii % string_of_mismatch) cases))

and string_of_mismatch (p, e) =
  string_of_expectation e ^ " under " ^ string_of_path p

type jin = Kojson_private.jin
let default_warn = ref (fun _ _ -> ())
let jin_of_json ?(warn = !default_warn) ?(path = []) v = (warn, path, v)
let jin_path (_, p, _) = List.rev p

type jain = Kojson_private.jain
let jain_path (_, p, _) = List.rev p
