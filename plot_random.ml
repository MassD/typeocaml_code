open Printf
open Camlimages
open Core.Std

let print_rgb {Color.r;g;b} =
  printf "r = %d, g = %d, b = %d\n" r g b

let bitmap_to_img ~bitmap ~w ~h = 
  let img = Rgb24.create w h in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      (*print_rgb bitmap.(i).(j);*)
      Rgb24.set img i j bitmap.(i).(j)
    done
  done;
  img

let save_img ~img ~filename = Jpeg.save filename [] (Images.Rgb24 img)

let ran_float_0_1 ~ran_f =
  fun ~w ~h ->
    let v = (Float.of_int (w*h)) *. ran_f() |> Float.to_int in 
    (v mod w, v / w)

let ran_int ~ran_f =
  fun ~w ~h ->
    let v = ran_f (w*h) in
    (v mod w, v / w)

let random_plot ~filename ~ran_f ~w ~h ~n =
  let darker c = if c-30 >= 0 then c-30 else 0 in
  let bitmap = Array.make_matrix ~dimx:w ~dimy:h {Color.r = 255; g = 255; b = 255} in
  let runs = w * h * n in
  let prog = ref 0 in
  let step = runs / 100 in
  let save_plot () = save_img ~filename ~img:(bitmap_to_img ~bitmap ~w ~h) in
  for i = 1 to runs do
    if i mod step = 0 then (prog := !prog + 1; printf "%d%%\n%!" !prog;save_plot());
    let x,y = ran_f ~w ~h in
    let {Color.r;g;b} = bitmap.(x).(y) in
    bitmap.(x).(y) <- {Color.r = darker r;g = darker g;b = darker b};
    (*printf "%d, x = %d, y = %d \n" i x y;*)
  done;
  save_plot()

let std_ran_int = ran_int ~ran_f:(fun n -> Random.self_init(); Random.int n)
let std_ran_float = ran_float_0_1 ~ran_f:(fun () -> Random.self_init(); Random.float 1.)

let _ = random_plot ~filename:"random_plot_int_tiny.jpg" ~ran_f:std_ran_int ~w:200 ~h:200 ~n:5
(*let _ = random_plot ~filename:"random_plot_float.jpg" ~ran_f:std_ran_float ~w:1024 ~h:1024 ~n:5*)
