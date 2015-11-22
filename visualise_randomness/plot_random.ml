open Camlimages

let random_to_bitmap ~ran_f ~w ~h ~n =
  let bitmap = Array.make_matrix w h {Color.r = 255; g = 255; b = 255} in
  let to_row_col ~w ~v = v / w, v mod w in
  let darker c = if c-30 >= 0 then c-30 else 0 in
  for i = 1 to w * h * n do
    let row, col = to_row_col ~w ~v:(ran_f (w * h)) in
    let {Color.r;g;b} = bitmap.(x).(y) in
    bitmap.(x).(y) <- {Color.r = darker r;g = darker g;b = darker b};
  done;
  bitmap

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
  let bitmap = random_to_bitmap ~ran_f ~w ~h ~n in
  let img = bitmap_to_img ~bitmap in
  save_img ~filename ~img

let decimal_only f = f -. (floor f)
let ran_via_time bound = ((Unix.gettimeofday() |> decimal_only) *. 100000000. |> int_of_float) mod bound

let _ = random_plot ~filename:"random_plot_time.jpg" ~ran_f:ran_via_time ~w:1024 ~h:1024 ~n:5


(* let _ = random_plot ~filename:"random_plot_int.jpg" ~ran_f:Random.int ~w:1024 ~h:1024 ~n:5 *)
