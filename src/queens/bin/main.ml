(* file: main.ml
 * author: Bob Muller
 *
 * CSCI 1103 Computer Science 1 Honors
 *
 * This program solves the 8-queens problem.
 *
 * Compile and run:
 *
 * > cd src
 * > dune exec bin/main.exe
 *)
let imageSide = 100.               (* determined by size of crown.png *)
let boardSize = 8. *. imageSide

let emptyBoardImage = Image.rectangle boardSize boardSize Color.white

let whiteSquare = Image.rectangle imageSide imageSide Color.darkGray
let graySquare  = Image.rectangle imageSide imageSide Color.gray20
let crown       = Image.readImage "./png/crown.png"

type state = Stop | Go

(* toggle : state -> state *)
let toggle state =
  match state with
  | Stop -> Go
  | Go -> Stop

type piece  = Queen | Empty
type board = (piece array) array
type stack = (int * int) Stack.t

type model = { board : board
             ; stack : stack
             ; state : state
             ; row : int
             ; col : int
             ; solved : bool
             }

let initialModel = { board = Array.make_matrix 8 8 Empty
                   ; stack = Stack.create ()
                   ; state = Stop
                   ; row = 0
                   ; col = 0
                   ; solved = false
                   }

(* next : int * int -> int * int *)
let next (row, col) =         (* starts at (0, 0); returns *)
  if col < 7 then             (* (8, 0) when all done.     *)
    (row, col + 1)
  else
    (row + 1, 0)

(* attacksFromAbove : int * int -> int * int -> board -> bool *)
let attacksFromAbove (row1, col1) (row2, col2) board =
  let hasAQueen  = board.(row1).(col1) = Queen in
  let sameColumn = col1 = col2 in
  let onDiagonal = (abs (row1 - row2)) = (abs (col1 - col2))
  in
  hasAQueen && (sameColumn || onDiagonal)

(* attackedFromAbove : int * int -> board -> bool *)
let attackedFromAbove (row, col) board =
  let rec repeat (row', col') =
    match row' = row with
    | true  -> false
    | false -> (match attacksFromAbove (row', col') (row, col) board with
		| true  -> true
		| false -> repeat (next(row', col')))
  in
  repeat (0, 0)

(* update : model -> model *)
let update ({board; stack; state; row; col} as model) =
  let rec solve (row, col) =
    match row = 8 with
    | true  -> { model with solved = true}
    | false ->
      (match attackedFromAbove (row, col) board with
		   | true  ->
         (match col < 7 with
			    | true  -> solve (row, col + 1)
			    | false -> backTrack model)
		   | false ->
         board.(row).(col) <- Queen;
			   Stack.push (row, col) stack;
			   { model with row=row+1; col=0})

  and backTrack model =
    match Stack.is_empty model.stack with
    | true  -> failwith "queens: there is no solution.\n"
    | false ->
      let (row', col') = Stack.pop model.stack
	    in
	    board.(row').(col') <- Empty;
	    match col' = 7 with
	    | true  -> backTrack model              (* keep popping ... *)
	    | false -> solve (next (row', col'))
  in
  match state with
  | Stop -> model
  | Go   -> solve (row, col)

(* view -> model -> Image.t *)
let view {board; stack} =
  let getImage piece row col =
    match piece with
    | Empty -> (if ((row + col) mod 2) = 0 then whiteSquare else graySquare)
    | Queen -> crown in
  let rec makeBoardImage (row, col) image =
    match row = 8 with
    | true  -> image
    | false ->
      let img = getImage board.(row).(col) row col in
      let (x, y) = ((float col) *. imageSide, (float row) *. imageSide) in
	    let newImage = Image.placeImage img (x, y) image
	    in
	    makeBoardImage (next (row, col)) newImage
  in
  makeBoardImage (0, 0) emptyBoardImage

(* handleMouse : model -> int -> int -> string -> model *)
let handleMouse model x y event =
  match event = "button_up" with
  | true  -> { model with state = toggle model.state }
  | false -> model

(* finished : model -> bool *)
let finished model = model.solved

let queens () =
  Animate.start
    initialModel
		~name:"Eight Queens!"
		~width:  boardSize
    ~height: boardSize
    ~onTick: update
    ~onMouse: handleMouse
		~rate:0.5
    ~view: view
		~stopWhen: finished
    ~viewLast: view

let () = queens ()
