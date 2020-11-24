(* file: main.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This program generates random sequences of nucleotides of length
   between 8 and 15 and writes them out to a file. Each sequence is
   unique.

   Usage:
   > cd src
   > dune exec bin/main.exe numberOfSequences outputFileName
*)
let genSym () =
  let syms = ['A'; 'T'; 'G'; 'C']
  in
  List.nth syms (Random.int 4)

(* genSeq : int -> string

   (genSeq n) generates a random sequence of nucleotides ranging in
   length up to n.
*)
let genSeq n =
  let rec loop n answer =
    match n = 0 with
    | true  -> Lib.implode answer
    | false -> loop (n - 1) (genSym () :: answer)
  in
  loop n []

(* Make an implementation of a map, the purpose of this map is to 
   avoid generating duplicate strings. *)
module M = Map.Make(String)

(* writeKeys : int -> string -> unit *)
let writeKeys n filename =
  let oc = open_out filename in
  let rec repeat n knownKeys =
    match n = 0 with
    | true  -> close_out oc
    | false ->
      let seq = genSeq (8 + Random.int 8)
      in
      match M.mem seq knownKeys with
      | true  ->
        Lib.pfmt "We've already seen %s\n" seq
      ; repeat n knownKeys
      | false ->
        let knownKeys = M.add seq () knownKeys
        in
        output_string oc (seq ^ "\n")
      ; repeat (n - 1) knownKeys
  in
  repeat n M.empty

let go () =
  let n = int_of_string (Sys.argv.(1))
  in
  writeKeys n (Sys.argv.(2))

let _ = go ()
