open Camlimages

let bitmap_to_img ~bitmap =
  let w = Array.length bitmap in
  let h = if w = 0 then 0 else Array.length bitmap.(0) in
  let img = Rgb24.create w h in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      Rgb24.set img i j bitmap.(i).(j)
    done
  done;
  img

let save_img ~filename ~img = Jpeg.save filename [] (Images.Rgb24 img)

let random_plot ~filename ~ran_f ~w ~h ~n =
  let darker c = if c-30 >= 0 then c-30 else 0 in
  let to_x_y ~w ~v = v mod w, v / w in
  let bitmap = Array.make_matrix w h {Color.r = 255; g = 255; b = 255} in
  for i = 1 to w * h * n do
    let x,y = to_x_y ~w ~v:(ran_f (w * h)) in
    let {Color.r;g;b} = bitmap.(x).(y) in
    bitmap.(x).(y) <- {Color.r = darker r;g = darker g;b = darker b};
  done;
  let img = bitmap_to_img ~bitmap in
  save_img ~filename ~img;;


(* initialize the random number generator *)
Random.self_init()

(* get a random gaussian using a Box-Muller transform, described
 * here http://en.wikipedia.org/wiki/Box-Muller_transform *)
let rec get_one_gaussian_by_box_muller () =
    (* Generate two uniform numbers from -1 to 1 *)
    let x = Random.float 2.0 -. 1.0 in
    let y = Random.float 2.0 -. 1.0 in
    let s = x*.x +. y*.y in
    if s > 1.0 then get_one_gaussian_by_box_muller ()
    else x *. sqrt (-2.0 *. (log s) /. s)

(* get a gaussian through oversampling and subtraction *)
let get_one_gaussian_by_summation () =
    let rec add_one limit count so_far =
        if count==limit then so_far
        else add_one limit (count+1) (so_far +. (Random.float 1.0)) in
    (add_one 12 0 0.0) -. 6.0

let get_one_gaussian = get_one_gaussian_by_box_muller

let gaussian_rand n = (float_of_int n) *. (get_one_gaussian()) |> int_of_float |> abs

let ran_gen_via_time bound = (Unix.time() |> int_of_float) mod bound;;

let _ = random_plot ~filename:"random_plot_gaussian.jpg" ~ran_f:ran_gen_via_time ~w:1024 ~h:1024 ~n:5


(* let _ = random_plot ~filename:"random_plot_int.jpg" ~ran_f:Random.int ~w:1024 ~h:1024 ~n:5 *)
