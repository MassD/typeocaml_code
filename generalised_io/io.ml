type 'a input = {pos: unit -> int;
	      seek: int -> unit;
	      content: unit -> 'a;
	      len: int;
	     }

let input_of_string str =
  let pos = ref 0 in
  {pos = (fun () -> !pos);
   seek = (fun i -> pos:=i);
   content = (fun ()-> str.[!pos]);
   len = String.length str;}

let input_words_of_string str =
  let rec split acc w i =
    if i >= String.length str then List.rev acc
    else if str.[i] = ' ' && Buffer.length w > 0 then split ((Buffer.contents w)::acc) (Buffer.create 8) (i+1)
    else if str.[i] = ' ' then split acc w (i+1)
    else (Buffer.add_char w str.[i]; split acc w (i+1))
  in 
  let wa = split [] (Buffer.create 8) 0 |> Array.of_list in
  let wa_len = Array.length wa in
  let pos = ref 0 in
  {pos = (fun () -> !pos);
   seek = (fun i -> if i > wa_len || i < 0 then raise (Invalid_argument "Cannot seek") else pos := i);
   content = (fun () -> wa.(!pos));
   len = wa_len;
  }
