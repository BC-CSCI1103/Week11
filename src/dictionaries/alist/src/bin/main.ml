(* file: test.ml
   author: Bob Muller

   CSCI 1103 Computer Science 1 Honors

   This file is a test harness for a few different
   implementations of maps.

Usage:
   > cd src
   > dune exec bin/main.exe inputfile searches

   or, for timing information:

   > time dune exec bin/main.exe inputfile searches

   where
     inputfile - the name of the file containing the
                 keys to be entered into the map
     searches  - the number of random key searches

   NB: There are a couple of inputfiles ../keys/random.txt
   and ../keys/ordered.txt
*)
open Map

let maxSize = 100000

let readSeqs file a =
  let ic = open_in file in
  let rec loop i =
    (try
       let _ = a.(i) <- input_line ic
       in
       loop (i + 1)
     with
       End_of_file ->
       close_in ic
       ; a)
  in
  loop 0

let notify i =
  if (i mod 1000) = 0 then
    let _ = Lib.pfmt "%d ...\n" i
    in
    flush stdout
  else
  ()

(* The go function reads a sequence of keys from
   the inputfile and stores them in an array. Then
   the keys are inserted into a key/value map.
   Finally, some number of the keys are sought in
   the map.
*)
let go () =
  let file = Sys.argv.(1) in
  let n = int_of_string (Sys.argv.(2)) in
  let a = readSeqs file (Array.make maxSize "") in
  let rec makeMap i map =
    match i = (Array.length a) with
    | true  -> map
    | false -> makeMap (i + 1) (Map.add a.(i) () map) in
  let map = makeMap 0 Map.empty in
  let rec loop i =
    match i = n with
    | true  -> Lib.pfmt "All done.\n"
    | false ->
      let _ = notify i in
      let j = Random.int maxSize in
      let _ = Map.find a.(j) map
      in
      loop (i + 1)
  in
  loop 0

let _ = go ()
