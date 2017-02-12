(* Copyright (C) 2013--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Kojson

(** Pattern Construction. *)

(** Patterns for Matching JSON Trees. *)
module K : sig

  (** {2 Primitive Patterns} *)

  val literal : json -> jin -> unit
  (** [literal v] matches only the value [v]. *)

  val convert : string -> (json -> 'a) -> jin -> 'a
  (** [convert type_name f] performs the conversion [f], intercepting
      [Failure] and [Invalid_argument] and re-raises them as a conversion
      error informing that a value of [type_name] was expected. *)

  val convert_string : string -> (string -> 'a) -> jin -> 'a
  (** [convert_string type_name f] performs a conversion to string followed by
      [f], intercepting [Failure] and [Invalid_argument], re-raised as
      convertion error claiming [type_name] was expected. *)

  val any : jin -> json
  (** [json] matches any value and returns it. *)

  val null : jin -> unit
  (** [null] matches [`Null]. *)

  val bool : jin -> bool
  (** [bool] matches a value of the form [`Bool b] and returns [b]. *)

  val int : jin -> int
  (** [int] matches a value of the form [`Int i] and returns [i]. *)

  val float : jin -> float
  (** [float] matches a value of the form [`Float x] and returns [x]. *)

  val string : jin -> string
  (** [string] matches a value of the form [`String s] and returns [s]. *)

  val string_enum : (string * 'a) list -> jin -> 'a
  (** [string_enum lxs] matches any of the keys of the association list [lxs]
      and returns the corresponding value. *)

  (** {2 Pattern Combinators} *)

  val list : (jin -> 'a) -> jin -> 'a list
  (** [list f] matches [`List vs] if [f] matches [v] for each [v] in [vs], and
      returns the list of values [f v]. *)

  val array : (jin -> 'a) -> jin -> 'a array
  (** [array f] matches the same inputs as [list f], but returns an array
      instead of a list. *)

  val assoc : (jain -> 'a) -> jin -> 'a
  (** [assoc f] matches [`Assoc vs] if [f] matches [vs] and passes though the
      computed value [f vs].  The pattern [f] can be constructed using the
      {!Ka} module. *)

  val assoc_or_null : (jain -> 'a) -> jin -> 'a
  (** [assoc_or_null f] matches the same inputs as [assoc f], and also matches
      [`Null] and [`List []] provided [f] matches the empty association. *)

  val first : (jin -> 'a) list -> jin -> 'a
  (** [first [f₁; …; fₙ]] matches a value which is matched by at least one of
      the [fᵢ] patters, and returns the result of the first match. *)

end

(** Patterns for Matching Labelled Collections of JSON Trees.  These combine
    patterns indexed by labels into patterns which match the remainder of an
    association. *)
module Ka : sig

  val any : jain -> (string * json) list
  (** [any] matches any remaining bindings and returns them. *)

  val drop : string list -> jain -> jain
  (** [drop ls] removes bindings of which the key is in [ls]. *)

  val empty : 'a -> jain -> 'a
  (** [empty x] matches the empty association and returns [x]. *)

  val stop : 'a -> jain -> 'a
  (** [stop x] matches any association and returns [x], but warns about
      present labels. *)

  val fold : (string -> jin -> 'a -> 'a) -> jain -> 'a -> 'a
  (** [fold f] matches any association such that [f lᵢ] matches the value [vᵢ]
      bound to [lᵢ] for each label.  The composition [f lₙ vₙ ∘ ⋯ ∘ f l₁ v₁]
      is returned. *)

  val iter : (string -> jin -> unit) -> jain -> unit
  (** [iter f] matches any association such that [f lᵢ] matches the value
      [vᵢ] bound to [lᵢ] for each label. *)

  val map : (string -> jin -> 'a) -> jain -> 'a list
  (** [map f] matches any association such that [f lᵢ] matches the value [vᵢ]
      bound to [lᵢ] for each label.  The list [[f l₁ v₁; …; f lₙ vₙ]] is
      returned. *)

  val first : (jain -> 'a) list -> jain -> 'a
  (** [first [f₁; …; fₙ]] matches an association which is matched by [fᵢ] for
      some [i], and returns the result of the first match. *)
end

val ( ^: ) : string -> (jin -> jain -> 'a) -> jain -> 'a
(** [l^: f] matches an association with a label [l] such that [f] matches
    the mapping of [l] and returns a pattern which matches the remainder.
    This combinator can be chained into patterns matching a complete
    association:
    {[
      open Unprime

      type connspec = {uri : string; timeout : int}

      let decode_connspec =
        let open Kojson in
        K.assoc begin
          "uri"^: K.string @> fun uri ->
          "timeout"^: K.int @> fun timeout ->
          Ka.stop {uri; timeout}
        end
    ]}
    The above [@>] is the reversed composition operator [(fun f g x -> g (f
    x))]. *)

val ( ^?: ) : string -> (jin option -> jain -> 'a) -> jain -> 'a
(** [l^?: f] matches an association which either has no mapping for [l] and is
    matched by [f None] or which maps [l] to some [v] with remainder [lvs]
    such that [f (Some v) lvs] verifies a match for both [v] and [lvs].  An
    [Option.map] function is convenient for constructing patterns on options:
    {[
      let decode_certspec =
        let open Kojson in
        K.assoc begin
          "certkey"^: K.string @> fun certkey ->
          "cert"^?: Option.map K.string @> Option.get_or certkey @> fun cert ->
          Ka.drop ["comment"] @>
          Ka.stop (certkey, cert)
        end
    ]} *)
