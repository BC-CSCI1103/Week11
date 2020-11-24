(* file: map.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This is an implementation of maps using OCaml's
   standard library, a balanced search tree.
*)
module M = Map.Make(String)

type key = string

type 'a t = 'a M.t

let empty = M.empty
let find  = M.find
let add   = M.add
