(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

(** Pattern Domains and Supporting Definitions. *)

(** {2 Prerequisites} *)

type json =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * json) list
  | `List of json list ]
(** The JSON tree, compatible with {!Yojson.Basic.json}. *)

type path = [`Label of string | `Index of int] list
(** A path of crossed association labels and array indices, used to locate a
    pattern mismatch for better error reporting. *)

val string_of_path : path -> string
(** Convert a label and index path to a human readable string. *)

type expectation =
  [ `expecting_value of json
  | `expecting_values of json list
  | `expecting_type of string
  | `expecting_label of string
  | `not_expecting_labels of string list
  | `either of (path * expectation) list ]
(** Details of what was expected at the location where a pattern didn't
    match. *)

exception Mismatch of path * expectation
(** Exception raised to indicate a mismatch of a JSON value.  The first
    parameter is a path leading up to the error, and the second parameter
    explains the expected value. *)

val string_of_mismatch : path * expectation -> string
(** Format a explanation of why a pattern did not match. *)

val default_warn : (path -> string -> unit) ref
(** This is the default warning handler used by {!jin_of_json}. *)


(** {2 Pattern Domains}

    The following types form the domain of patterns, which can be built up
    from the definitions in {!Kojson_pattern}. *)

type jin = Kojson_private.jin
(** A JSON value paired with a path and a custom warning callback to
    facilitate error reporting.  A {e pattern} is a function of type [jin ->
    'a] which throws {!Mismatch} if it does not match its argument. *)

val jin_of_json : ?warn: (path -> string -> unit) -> ?path: path -> json -> jin
(** Pair a JSON value with a path for use during matching.

    @param path The root path of JSON tree, if not [[]].

    @param warn A callback to record warnings about unused labels.  The
    default is {!default_warn}. *)

val jin_path : jin -> path

type jain = Kojson_private.jain
(** A wrapper for an opened JSON association paired with a path and a custom
    warning callback to facility error reporting.  An {e association pattern}
    is a function of type [jain -> 'a] which throws {!Mismatch} if it does not
    match its argument.  *)

val jain_path : jain -> path
