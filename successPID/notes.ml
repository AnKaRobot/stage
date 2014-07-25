open Cortex;;

let resistdouble = return 
~on_proposal: (fun (x, y) (xn, yn) ->
		   (x, yn))
(0, 0);;

set resistdouble (1, 1);;


let pid = 
  let alpha = 0.5 in
  let beta = 0.3 in
  let gamma = 0.2 in
  let tau = 0.2 in
  let on_proposal (tp, mp, rp, dp, sp, _) (t, m, r, _, _, _) =
    let () = print_string "propose" in
    let dt = float_of_int (t - tp) in

    let d = if (t > tp) then
		  (m -. mp) /. dt 
	    else dp
    in
    let s =  if (t > tp) then
               (* simplification faux si dt > 1, a revoir *)
	       tau *.  (r -. m) +. ( 1. -. tau) *. sp
	     else 
	       sp
    in
    let () = Printf.printf "r = %f m = %f\n" r m in
    let c = alpha *. (r -. m) +. beta *. s +. gamma *. d in
    let () = Printf.printf "c = %f\n" c in
    if (t > tp) then
      (t, m, r, d, s, c)
    else
      (tp, m, r, dp, sp, c)
  in
  let equality (tp, mp, rp, dp, sp, cp) (t, m, r, d, s, c) =
    (tp, mp, rp, dp, sp, cp) = (t, m, r, d, s, c)
  in
  return 
    ~on_proposal
    ~equality
    (0, 0., 0., 0., 0., 0.);;

let setpid (t,m,r) = let (_, _, _, d, s, c) = get pid in
		     set pid (t, m, r,  d, s, c);;
let getpid () = let (_, _, _, _, _, c) = get pid in c;;

(* 
si t, m, r ne change pas on ne recalcule rien.
si t change on recalcule "tout" (y compris s et d)
si t ne change pas, m change ou r change on recalcule la part proportionnelle de c et on ne retient pas m, on retient r.
 *)
