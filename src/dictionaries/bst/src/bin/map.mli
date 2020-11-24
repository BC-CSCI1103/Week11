(* file: map.mli
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This is a specification of a very simple map
   with string keys.
*)
type key = string
type 'a t

val empty : 'a t
val find  : key -> 'a t -> 'a
val add   : key -> 'a -> 'a t -> 'a t
