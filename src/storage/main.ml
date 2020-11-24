(* file: main.ml

   CSCI 1103 Computer Science 1 Honors

   A couple of examples exhibiting storage diagrams.
   These examples, are depicted in the pptx slides.
*)

(* g : int * int * int -> (int * int * int) list *)
let g a =
  let b = [a; a]
  in
  b

(* f : unit -> (int * int * int) list *)
let f () =
  let x = (2, 3, 4) in
  let y = g x               (* 1 *)
  in
  y                         (* 2 *)


(* An example generating garbage in the heap.

 h : unit -> int *)
let h () =
  let x = (2, 3, 4)
  in
  12
