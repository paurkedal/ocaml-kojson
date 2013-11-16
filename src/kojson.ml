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

open Printf
open Prime
open Unprime_list

module String_set = Set.Make (String)
module String_map = Prime_map.Make (String)

type json = Yojson.Basic.json

type path = [`Label of string | `Index of int] list

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
	(List.map (String.uncapitalize *< string_of_mismatch) cases))

and string_of_mismatch (p, e) =
  string_of_expectation e ^ " under " ^ string_of_path p

type value = (path -> string -> unit) * path * json
type assoc = (path -> string -> unit) * path * json String_map.t

let default_warn = ref (fun _ _ -> ())

let of_json ?(warn = !default_warn) ?(path = []) v = (warn, path, v)

let path (_, p, _) = List.rev p

let literal v (warn, p, v') =
  if v <> v' then raise (Mismatch (List.rev p, `expecting_value v))

let convert tn f (_, p, v) =
  try f v with
  | Failure msg -> raise (Mismatch (List.rev p, `expecting_type tn))

let json (_, _, v) = v

let null = function
  | _, _, `Null -> ()
  | _, p, _ -> raise (Mismatch (List.rev p, `expecting_type "null"))

let bool = function
  | _, _, `Bool b -> b
  | _, p, _ -> raise (Mismatch (List.rev p, `expecting_type "bool"))

let int = function
  | _, _, `Int i -> i
  | _, p, _ -> raise (Mismatch (List.rev p, `expecting_type "int"))

let float = function
  | _, _, `Float x -> x
  | _, p, _ -> raise (Mismatch (List.rev p, `expecting_type "float"))

let string = function
  | _, _, `String s -> s
  | _, p, _ -> raise (Mismatch (List.rev p, `expecting_type "string"))

let string_enum lxs (warn, p, v) =
  try
    begin match v with
    | `String s -> List.assoc s lxs
    | _ -> raise Not_found
    end
  with Not_found ->
    let ls = List.map (fun (l, _) -> `String l) lxs in
    raise (Mismatch ([], `expecting_values ls))

let list f = function
  | warn, p, `List vs -> List.mapi (fun i v -> f (warn, `Index i :: p, v)) vs
  | warn, p, _ -> raise (Mismatch (List.rev p, `expecting_type "list"))

let assoc f = function
  | warn, p, `Assoc lvs ->
    f (warn, p, List.fold (uncurry String_map.add) lvs String_map.empty)
  | warn, p, _ -> raise (Mismatch (List.rev p, `expecting_type "assoc"))

let first fs (warn, p, v) =
  let rec loop misses = function
    | f :: fs ->
      begin try f (warn, p, v) with
      | Mismatch (pi, msgi) -> loop ((pi, msgi) :: misses) fs
      end
    | [] ->
      raise (Mismatch (List.rev p, `either (List.rev misses))) in
  loop [] fs

let ( ^: ) l f (warn, p, lvs) =
  let g =
    try f (warn, `Label l :: p, String_map.find l lvs)
    with Not_found -> raise (Mismatch (List.rev p, `expecting_label l))
  in g (warn, p, String_map.remove l lvs)

let ( ^?: ) l f (warn, p, lvs) =
  let vo, lvs' =
    try (Some (warn, `Label l :: p, String_map.find l lvs),
	 String_map.remove l lvs)
    with Not_found -> (None, lvs) in
  f vo (warn, p, lvs')

module Assoc = struct

  let path (_, p, _) = List.rev p

  let json (_, _, lvs) = String_map.bindings lvs

  let drop ls (warn, p, lvs) = (warn, p, List.fold String_map.remove ls lvs)

  let empty x (warn, p, lvs) =
    begin if not (String_map.is_empty lvs) then
      let ls = String_map.fold (fun k _ ks -> k :: ks) lvs [] in
      raise (Mismatch (List.rev p, `not_expecting_labels ls))
    end; x

  let stop x (warn, p, lvs) =
    begin if not (String_map.is_empty lvs) then
      let ls = String_map.fold (fun k _ ks -> k :: ks) lvs [] in
      ksprintf (warn (List.rev p)) "Redundant label%s %s in association."
	(if List.length ls > 1 then "s" else "")
	(String.concat ", " ls)
    end; x

  let fold f (warn, p, lvs) =
    String_map.fold (fun l v -> f l (warn, `Label l :: p, v)) lvs

  let iter f (warn, p, lvs) =
    String_map.iter (fun l v -> f l (warn, `Label l :: p, v)) lvs

  let map f (warn, p, lvs) =
    String_map.fold (fun l v acc -> f l (warn, `Label l :: p, v) :: acc) lvs []

  let first fs (warn, p, lvs) =
    let rec loop misses = function
      | f :: fs ->
	( try f (warn, p, lvs)
	  with Mismatch (pi, msgi) -> loop ((pi, msgi) :: misses) fs )
      | [] ->
	raise (Mismatch (List.rev p, `either (List.rev misses))) in
    loop [] fs
end
