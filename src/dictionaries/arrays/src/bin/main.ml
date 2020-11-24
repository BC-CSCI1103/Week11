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

   NB: There are a couple of 100,000 line inputfiles
   ../keys/random.txt and
   ../keys/ordered.txt
*)
let readSeqs file =
  let ic = open_in file in
  let rec loop n result =
    (try
       let sequence = input_line ic
       in
       loop (n + 1) (sequence :: result)
     with
     | End_of_file ->
       close_in ic
     ; (Array.of_list (List.rev result), n))
  in
  loop 0 []

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
  let nSearches = int_of_string (Sys.argv.(2)) in
  let (a, nKeys) = readSeqs file in
  let rec makeMap i map =
    match i = nKeys with
    | true  -> map
    | false -> makeMap (i + 1) (Map.add a.(i) "" map) in
  let map = makeMap 0 Map.empty in
  let rec findLoop i =
    match i = nSearches with
    | true  -> Lib.pfmt "All done.\n"
    | false ->
      let _ = notify i in
      let j = Random.int nKeys in
      let _ = Map.find a.(j) map
      in
      findLoop (i + 1)
  in
  Lib.pfmt "Ready. Look for %d keys, dictionarySize = %d\n" nSearches nKeys;
  findLoop 0

let _ = go ()
