(* file: map.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This is an implementation of maps using binary
   search trees.
*)

type key = string
type 'a t = Empty
          | Node of { key   : key
                    ; value : 'a
                    ; left  : 'a t
                    ; right : 'a t
                    }

let empty = Empty

let rec find searchKey map =
  match map with
  | Empty -> failwith "no key"
  | Node {key; value; left; right} ->
    let result = compare searchKey key
    in
    match result = 0 with
    | true  -> value
    | false ->
      (match result = -1 with
       | true  -> find searchKey left
       | false -> find searchKey right)

let rec add newKey newValue map =
  match map with
  | Empty -> Node { key   = newKey
                  ; value = newValue
                  ; left  = Empty
                  ; right = Empty
                  }
  | Node {key; value; left; right} ->
    let result = compare newKey key
    in
    match result = 0 with
    | true  -> Node { key
                    ; value = newValue
                    ; left
                    ; right
                    }
    | false ->
      (match result = -1 with
       | true  ->
         Node { key
              ; value
              ; left = add newKey newValue left
              ; right
              }
       | false ->
         Node { key
              ; value
              ; left
              ; right = add newKey newValue right
              })
