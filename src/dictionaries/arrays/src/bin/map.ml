(* file: map.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This is an implementation of dictionaries using parallel arrays
   with comparable keys. The advantage is that we can use binary
   search for the find operation.
*)
type key = string
type value = string

type t = { keys   : key array
         ; values : string array  (* option to get polymorphism *)
         ; size   : int
         }

(* We'll use the alias dictionary for the type 'a t *)

(* empty : dictionary *)
let empty = { keys   = Array.make 0 ""
            ; values = Array.make 0 ""
            ; size   = 0
            }

(* find : key -> dictionary -> value *)
let find key map =
  let rec loop lo hi =
    match lo > hi with
    | true  -> failwith (Lib.fmt "key %s not found in dictionary." key)
    | false ->
      let middle = (lo + hi) / 2 in
      let comparison = compare key map.keys.(middle)
      in
      match comparison = 0 with
      | true  -> map.values.(middle)
      | false ->
        (match comparison < 0 with
         | true  -> loop lo (middle - 1)
         | false -> loop (middle + 1) hi)
  in
  loop 0 (map.size - 1)


(******** Helper functions for add  ***********)

(* findIndex : key -> dictionary -> int *)
let findIndex key map =
  let rec loop i =
    match i = map.size || compare key map.keys.(i) < 0 with
    | true  -> i
    | false -> loop (i + 1)
  in
  loop 0

(* resize : dictionary -> dictionary *)
let resize map =
  let n = max 1 (Array.length map.keys * 2) in
  let newMap = { keys   = Array.make n ""
               ; values = Array.make n ""
               ; size   = map.size
               }
  in
  for i = 0 to Array.length map.keys - 1 do
    newMap.keys.(i)   <- map.keys.(i) ;
    newMap.values.(i) <- map.values.(i)
  done ;
  newMap

(* maybeResize : dictionary -> dictionary *)
let maybeResize map =
  if map.size < Array.length map.keys - 1 then map else resize map

(* add : key -> value -> dictionary -> dictionary *)
let add key value map =
  let map = maybeResize map in
  let insertionSpot = findIndex key map
  in
  for i = 0 to (map.size - insertionSpot) - 1 do
    map.keys.(map.size - i)   <- map.keys.(map.size - i - 1);
    map.values.(map.size - i) <- map.values.(map.size - i - 1)
  done ;
  map.keys.(insertionSpot)   <- key ;
  map.values.(insertionSpot) <- value ;
  { keys = map.keys
  ; values = map.values
  ; size = map.size + 1
  }
