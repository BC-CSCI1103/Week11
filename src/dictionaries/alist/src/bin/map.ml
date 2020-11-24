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

let rec find key map =
  match map with
  | [] -> failwith "no key"
  | (key1, value) :: map ->
    let result = compare key key1
    in
    (if result = 0 then
       value
     else
       find key map)

let add key value map = (key, value) :: map
