(* file: tessellation.ml
 * author: Bob Muller
 * date: January 1, 2015
 *
 * Various simple tesselations of the plane.
 *
 * depends: Asai/Uehara Universe Library
 * compile: ./make
 * run: ./tessellation
*)
type bmp = Cairo.Image.data32

let rows bmp = Bigarray.Array2.dim1 bmp
let cols bmp = Bigarray.Array2.dim2 bmp

(* getPixel : bmp -> int * int -> Color.t *)
let getPixel bmp (row, col) =
  let rgba = bmp.{row, col} in
  let int255 = Int32.of_int 255 in
  let a = Int32.to_int (Int32.logand rgba int255) in
  let rgb = Int32.shift_right_logical rgba 8 in
  let b = Int32.to_int (Int32.logand rgb int255) in
  let rg = Int32.shift_right_logical rgb 8 in
  let g = Int32.to_int (Int32.logand rg int255) in
  let r = Int32.to_int (Int32.shift_right_logical rg 8)
  in
  Color.makeColor ~alpha:a r g b

(* setPixel : bmp -> int * int -> Color.t -> unit *)
let setPixel bmp (row, col) color =
  let colorToInt32 color =
    let (r, g, b, a) = Color.to_rgba color
    in
    Int32.of_int (((a * 256 + r) * 256 + g) * 256 + b)
  in
  bmp.{row, col} <- (colorToInt32 color)

let width  = 800.                (* window width *)
let height = 571.                (* window height *)

type model = { image : Image.t
             ; width : float
             ; height : float
             }

let initialModel () =
  match Array.length Sys.argv != 4 with
  | true  -> Lib.pfmt "This app needs a png file & float width & height.\n"
           ; exit 0
  | false ->
    { image  = Image.readImage (Sys.argv.(1))
    ; width  = float_of_string (Sys.argv.(2))
    ; height = float_of_string (Sys.argv.(3))
    }

(* view : model -> Image.t *)
let view model = model.image

(* zoomPixel : bmp -> bmp -> int -> int -> unit *)
let zoomPixel oldbmp newbmp i j =
  let pixel = getPixel oldbmp (i, j) in
  let (a, r, g, b) = Color.to_rgba pixel in
  let pixel = Color.makeColor ~alpha:255 r g b
  in
  for i' = 2 * i to 2 * i + 1 do
    for j' = 2 * j to 2 * j + 1 do
      setPixel newbmp (i', j') pixel
    done
  done

(* zoomIn : Image.t -> Image.t *)
let zoomIn image width height =
  let oldbmp = Image.toBitmap image in
  let newbmp = Image.toBitmap (Image.empty width height)
  in
  for row = 0 to (rows oldbmp) - 1 do
    for col = 0 to (cols oldbmp) - 1 do
        zoomPixel oldbmp newbmp row col
      done
    done
    ; Image.fromBitmap newbmp

(* handleMouse : model -> float -> float -> string -> model *)
let handleMouse model _ _ event =
  match event = "button_up" with
  | true ->
    let width = model.width *. 2.0 in
    let height = model.height *. 2.0
    in
    { image = zoomIn model.image width height
    ; width
    ; height
    }
  | false -> model

(*********************************************************************)

(* animation start *)
let go () =
  Animate.start (initialModel ())
    ~name:"Zoomer"
    ~view: view
    ~onMouse: handleMouse
    ~width:width
    ~height:height

let s = go ()
