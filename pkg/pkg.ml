#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let () = Pkg.describe ~licenses "kojson" @@ fun c ->
  Ok [
    Pkg.mllib ~api:["Kojson"; "Kojson_pattern"] "src/kojson.mllib";
  ]
