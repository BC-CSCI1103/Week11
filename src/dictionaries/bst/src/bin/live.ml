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

let rec find searchKey map = failwith "No"

let rec add newKey newValue map = failwith "No"
