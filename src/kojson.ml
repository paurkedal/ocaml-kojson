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

let on_warning = ref (fun s -> ())
let warn_f fmt = Printf.ksprintf (fun s -> !on_warning s) fmt

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

type value = path * json
type assoc = path * json String_map.t

let of_json ?(path = []) v = (path, v)
let path (p, _) = p

let literal v (p, v') =
  if v <> v' then raise (Mismatch (List.rev p, `expecting_value v))

let convert tn f (p, v) =
  try f v with
  | Failure msg -> raise (Mismatch (List.rev p, `expecting_type tn))

let json (p, v) = v

let null = function
  | _, `Null -> ()
  | p, _ -> raise (Mismatch (List.rev p, `expecting_type "null"))

let bool = function
  | _, `Bool b -> b
  | p, _ -> raise (Mismatch (List.rev p, `expecting_type "bool"))

let int = function
  | _, `Int i -> i
  | p, _ -> raise (Mismatch (List.rev p, `expecting_type "int"))

let float = function
  | _, `Float x -> x
  | p, _ -> raise (Mismatch (List.rev p, `expecting_type "float"))

let string = function
  | _, `String s -> s
  | p, _ -> raise (Mismatch (List.rev p, `expecting_type "string"))

let string_enum lxs (p, v) =
  try
    begin match v with
    | `String s -> List.assoc s lxs
    | _ -> raise Not_found
    end
  with Not_found ->
    let ls = List.map (fun (l, _) -> `String l) lxs in
    raise (Mismatch ([], `expecting_values ls))

let list f = function
  | p, `List vs -> List.mapi (fun i v -> f (`Index i :: p, v)) vs
  | p, _ -> raise (Mismatch (List.rev p, `expecting_type "list"))

let assoc f = function
  | p, `Assoc lvs ->
    f (p, List.fold (uncurry String_map.add) lvs String_map.empty)
  | p, _ -> raise (Mismatch (List.rev p, `expecting_type "assoc"))

let first fs (p, v) =
  let rec loop misses = function
    | f :: fs ->
      begin try f (p, v) with
      | Mismatch (pi, msgi) -> loop ((pi, msgi) :: misses) fs
      end
    | [] ->
      raise (Mismatch (List.rev p, `either (List.rev misses))) in
  loop [] fs

let ( ^: ) l f (p, lvs) =
  let g =
    try f (`Label l :: p, String_map.find l lvs)
    with Not_found -> raise (Mismatch (List.rev p, `expecting_label l))
  in g (p, String_map.remove l lvs)

let ( ^?: ) l f (p, lvs) =
  let vo, lvs' =
    try (Some (`Label l :: p, String_map.find l lvs),
	 String_map.remove l lvs)
    with Not_found -> (None, lvs) in
  f vo (p, lvs')

module Assoc = struct

  let path (p, _) = p

  let drop ls (p, lvs) = (p, List.fold String_map.remove ls lvs)

  let empty x (p, lvs) =
    begin if not (String_map.is_empty lvs) then
      let ls = String_map.fold (fun k _ ks -> k :: ks) lvs [] in
      raise (Mismatch (List.rev p, `not_expecting_labels ls))
    end; x

  let stop x (p, lvs) =
    begin if not (String_map.is_empty lvs) then
      let ls = String_map.fold (fun k _ ks -> k :: ks) lvs [] in
      warn_f "Redundant label%s %s in association at %s."
	(if List.length ls > 1 then "s" else "")
	(String.concat ", " ls) (string_of_path (List.rev p))
    end; x

  let fold f (p, lvs) =
    String_map.fold (fun l v -> f l (`Label l :: p, v)) lvs

  let iter f (p, lvs) =
    String_map.iter (fun l v -> f l (`Label l :: p, v)) lvs

  let map f (p, lvs) =
    String_map.fold (fun l v acc -> f l (`Label l :: p, v) :: acc) lvs []

  let first fs (p, lvs) =
    let rec loop misses = function
      | f :: fs ->
	( try f (p, lvs)
	  with Mismatch (pi, msgi) -> loop ((pi, msgi) :: misses) fs )
      | [] ->
	raise (Mismatch (List.rev p, `either (List.rev misses))) in
    loop [] fs
end
