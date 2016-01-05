let kth_2_arrays k a b = 
  let sa = Array.length a and sb = Array.length b in 
  if k >= sa+sb then raise Not_found
  else 
    let rec kth k (la, ha) (lb, hb) =
      if la >= ha then b.(k+lb)
      else if lb >= hb then a.(k+la)
      else  
	let ma = (ha+la)/2 and mb = (hb+lb)/2 in
	match a.(ma) < b.(mb), k <= ma+mb-la-lb with
	| true, true -> kth k (la, ha) (lb, mb)
	| true, false -> kth (k-ma-1+la) (ma+1, ha) (lb, hb)
	| false, true -> kth k (la, ma) (lb, hb)
	| false, false -> kth (k-mb-1+lb) (la, ha) (mb+1, hb)
    in 
    kth k (0, sa) (0, sb)

let kth_union k a b = 
  let sa = Array.length a and sb = Array.length b in
  let rec kth k (a, la, ha) (b, lb, hb) =
    if la >= ha+1 then b.(k+lb)
    else if lb >= hb+1 then a.(k+la)
    else  
      let ma = (ha+la)/2 and mb = (hb+lb)/2 in
      match a.(ma) < b.(mb), k >= ma-la+1+mb-lb with
      | true, true -> kth (k-(ma-la+1)) (a, ma+1, ha) (b, lb, hb)
      | true, false -> kth k (a, la, ha) (b, lb, mb-1)
      | _ -> kth k (b, lb, hb) (a, la, ha)
  in 
  kth k (a, 0, sa-1) (b, 0, sb-1)

let a = [|3;7;16;25;26|]
let b = [|1;5;13;28;30;36;51;58|]

let k = 8

let _ = kth_union k a b
