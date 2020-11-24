(* file: map.mli
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This is a specification of a simple dictionary with string keys.
*)
type key = string
type value = string
type t

val empty : t
val find  : key -> t -> string
val add   : key -> value -> t -> t
