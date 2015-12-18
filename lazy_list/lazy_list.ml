type 'a lazy_list_t = Cons of 'a * (unit -> 'a lazy_list_t)

let hd (Cons (x, _)) = x
let tl (Cons (_, tl_f)) = tl_f() 

let rec nth (Cons (x, tl_f)) n = 
  if n = 1 then x
  else nth (tl_f()) (n-1)

let takeout ll n =
  let rec helper acc i (Cons (x, tlf)) =
    if i = n then List.rev acc
    else helper (x::acc) (i+1) (tlf())
  in 
  helper [] 0 ll

let rec map f (Cons (x, tl_f)) = Cons (f x, fun () -> map f (tl_f()))

let mapi f ll =
  let rec helper (Cons (x, tl_f)) i =
    Cons (f i x, fun () -> helper (tl_f()) (i+1))
  in 
  helper ll 0

let rec filter f (Cons (x, tl_f)) = 
  if f x then Cons (x, fun () -> filter f (tl_f())) 
  else filter f (tl_f()) 

let rec fold f init (Cons (x, tl_f)) =
  let acc = f init x in
  Cons (acc, fun () -> fold f acc (tl_f()))

let partition f ll = filter f ll, filter (fun x -> f x |> not) ll

let partition' f ll =
  let mapped = map (fun x -> f x, x) ll in
  let map_back ll = map (fun (_, x) -> x) ll in
  filter (fun (b, x) -> b) mapped |> map_back, filter (fun (b, x) -> not b) mapped |> map_back

let natures () = 
  let rec helper i = Cons (i, fun () -> helper (i+1))
  in 
  helper 1

let rec ints_from i = Cons (i, fun () -> ints_from (i+1))

let primes () =
  let rec helper p (Cons (x, tlf)) =
    Cons (p, fun () -> helper x (filter (fun y -> y mod p <> 0) (tlf())))
  in 
  helper 2 (ints_from 3)

let rec interleave (Cons (x, tfl)) ll2 = Cons (x, fun () -> interleave ll2 (tfl()))

let num_list n = 
  let rec helper nl = 
    Cons(nl, fun() -> (helper (n::nl)))
  in 
  helper [n]



let rec all_from l = Cons(l, fun() -> interleave (all_from (0::l)) (all_from (1::l)))


let evens = 
  let rec helper i = Cons(i, fun() -> helper (2*i)) in
  helper 2


let fibs = 
  let rec helper i j = Cons(i, fun() -> helper j (i+j))
  in 
  helper 1 1


let cycles l =
  let rec helper acc = function
      | [] -> Cons (None, fun() -> helper [] [])
      | hd::[] -> Cons (Some hd, fun() -> helper [] (List.rev (hd::acc)))
      | hd::tl -> Cons (Some hd, fun() -> helper (hd::acc) tl)
  in 
  helper [] l


let drop ll n =
  let rec helper (Cons(_, tlf) as l) i =
    if i = n then l
    else helper (tlf()) (i+1)
  in 
  helper ll 0

let unleave ll =
  let rec always_drop ll n = 
    let dropped = drop ll n in
    Cons(hd dropped, fun() -> always_drop dropped n)
  in 
  Cons(hd ll, fun() -> always_drop ll 2), Cons(hd (drop ll 1), fun() -> always_drop (drop ll 1) 2)

let rec unleave' (Cons (h, tf)) =
  let Cons (h', tf') = tf () in
  let t = tf' () in
  (Cons (h, fun () -> fst (unleave' t)), Cons (h', fun () -> snd (unleave' t)))


let alphabet =
  let rec helper i ab =
    if i = 26 then helper 0 (tl ab)
    else Cons((hd ab)^(Char.escaped (char_of_int (65+i))), fun() -> helper (i+1) ab)
  in 
  let rec abet() = helper 0 (Cons("", fun() -> abet())) in
  abet()

