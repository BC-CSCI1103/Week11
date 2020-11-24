(* file: map.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This is an implementation of maps using simple
   association lists:

   [(key, value); ...; (key, value)]
*)
type key = string

type 'a t = (key * 'a) list

let empty = []

let rec find key dictionary =
  match dictionary with
  | [] -> failwith "key not found"
  | (key', value) :: dictionary ->
    if key' = key then value else find key dictionary

let add key value map = (key, value) :: map
