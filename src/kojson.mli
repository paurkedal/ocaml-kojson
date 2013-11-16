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

(** Parser combinators for [Yojson.Basic.json] expressions. *)

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

type value
(** A JSON value paired with a path to facilitate error reporting.  In the
    following, a {e pattern} is a function of type [value -> 'a] which throws
    {!Mismatch} if it does not match its argument. *)

type assoc
(** A wrapper for an opened JSON association paired with a path to facility
    error reporting.  Below, an {e association pattern} is a function of type
    [assoc -> 'a] which throws {!Mismatch} if it does not match its
    argument. *)

val default_warn : (path -> string -> unit) ref
(** This can be set to a default warning handler to pass to {!of_json}. *)

val of_json : ?warn: (path -> string -> unit) -> ?path: path -> json -> value
(** Pair a JSON value with a path for use during matching.

    @param path The root path of JSON tree, if not [[]].

    @param warn A callback to record warnings about unused labels.  The
    default is {!default_warn}. *)

val path : value -> path


(** {2 Primitive Patterns} *)

val literal : json -> value -> unit
(** [literal v] matches only the value [v]. *)

val convert : string -> (json -> 'a) -> value -> 'a
(** [convert type_name f] performs the conversion [f], intercepting [Failure]
    and re-raises them as a conversion error informing that a value of
    [type_name] was expected. *)

val json : value -> json
(** [json] matches any value and returns it. *)

val null : value -> unit
(** [null] matches [`Null]. *)

val bool : value -> bool
(** [bool] matches a value of the form [`Bool b] and returns [b]. *)

val int : value -> int
(** [int] matches a value of the form [`Int i] and returns [i]. *)

val float : value -> float
(** [float] matches a value of the form [`Float x] and returns [x]. *)

val string : value -> string
(** [string] matches a value of the form [`String s] and returns [s]. *)

val string_enum : (string * 'a) list -> value -> 'a
(** [string_enum lxs] matches any of the keys of the association list [lxs]
    and returns the corresponding value. *)

(** {2 Pattern Combinators} *)

val list : (value -> 'a) -> value -> 'a list
(** [list f] matches [`List vs] if [f] matches [v] for each [v] in [vs], and
    returns the list of values [f v]. *)

val assoc : (assoc -> 'a) -> value -> 'a
(** [assoc f] matches [`Assoc vs] if [f] matches [vs] and passes though the
    computed value [f vs].  The pattern [f] can be constructed using the
    {!Assoc} module. *)

val first : (value -> 'a) list -> value -> 'a
(** [first [f₁; …; fₙ]] matches a value which is matched by either of the [fᵢ]
    patters, and returns the result of the first match. *)

(** {2 Association Pattern Combinators} *)

val ( ^: ) : string -> (value -> assoc -> 'a) -> assoc -> 'a
(** [l^: f] matches an association with a label [l] such that [f] matches
    the mapping of [l] and returns a pattern which matches the remainder.
    This combinator can be chained into patterns matching a complete
    association:
    {[
      open Unprime

      type connspec = {uri : string; timeout : int}

      let decode_connspec =
	let open Kojson in
	assoc begin
	  "uri"^: string *> fun uri ->
	  "timeout"^: int *> fun timeout ->
	  Assoc.stop {uri; timeout}
	end
    ]}
    The above [*>] is the reversed composition operator [(fun f g x -> g (f
    x))]. *)

val ( ^?: ) : string -> (value option -> assoc -> 'a) -> assoc -> 'a
(** [l^?: f] matches an association which either has no mapping for [l] and is
    matched by [f None] or which maps [l] to some [v] with remainder [lvs]
    such that [f (Some v) lvs] verifies a match for both [v] and [lvs].  An
    [Option.map] function is convenient for constructing patterns on options:
    {[
      let decode_certspec =
	let open Kojson in
	assoc begin
	  "certkey"^: string *> fun certkey ->
	  "cert"^?: Option.map string *> Option.get_or certkey *> fun cert ->
	  Assoc.drop ["comment"] *>
	  Assoc.stop (certkey, cert)
	end
    ]} *)

(** Pattern combinators for associations.  These combine patterns indexed by
    labels into patterns which match the remainder of an association. *)
module Assoc : sig

  val path : assoc -> path

  val json : assoc -> (string * json) list

  val drop : string list -> assoc -> assoc
  (** [drop ls] removes the labels [ls]. *)

  val empty : 'a -> assoc -> 'a
  (** [empty x] matches the empty association and returns [x]. *)

  val stop : 'a -> assoc -> 'a
  (** [stop x] matches any association and returns [x], but warns about
      present labels. *)

  val fold : (string -> value -> 'a -> 'a) -> assoc -> 'a -> 'a
  (** [fold f] matches any association such that [f lᵢ] matches the value [vᵢ]
      bound to [lᵢ] for each label.  The composition [f lₙ vₙ ∘ ⋯ ∘ f l₁ v₁]
      is returned. *)

  val iter : (string -> value -> unit) -> assoc -> unit
  (** [iter f] matches any association such that [f lᵢ] matches the value
      [vᵢ] bound to [lᵢ] for each label. *)

  val map : (string -> value -> 'a) -> assoc -> 'a list
  (** [map f] matches any association such that [f lᵢ] matches the value [vᵢ]
      bound to [lᵢ] for each label.  The list [[f l₁ v₁; …; f lₙ vₙ]] is
      returned. *)

  val first : (assoc -> 'a) list -> assoc -> 'a
  (** [first_assoc [f₁; …; fₙ]] matches an association which is matched by [fᵢ]
      for some [i], and returns the result of the first match. *)
end
