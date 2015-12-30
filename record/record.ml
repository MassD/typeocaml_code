(* Show how to update a reference without using the := operator. *)
let x = ref 1
let _ = x.contents <- 2

type ('a, 'b, 'c) t = {x:'a; y:'b; z:'c}
