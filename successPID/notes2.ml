open Cortex
open Cortex_lib

(* Thread efficient printf *)
let scream fmt = Printf.kfprintf flush stdout fmt

(* Convertion functions *)
let stri i = string_of_int i
let strf f = string_of_float f
let strb b = if b then "T" else "F"
let stri' i' = match i' with
| None -> "None"
| Some i -> "Some " ^ (stri i)

let str2 l r = "(" ^ l ^ ", " ^ r ^ ")"
let str2ib (i, b) = str2 (stri i) (strb b)
let str2ii (i1, i2) = str2 (stri i1) (stri i2)
let str2ii' (i, i') = str2 (stri i) (stri' i')
let str2if (i, f) = str2 (stri i) (strf f)  

let strl f l = "(" ^ (StringExtra.map_concat ~sep:", " f l) ^ ")"
let strlf fl = strl strf fl

let strii'_i ((i1, i'), i2) = str2 (str2ii' (i1, i')) (stri i2)

let strf6 (f1, f2, f3, f4, f5, f6) = strlf [f1; f2; f3; f4; f5; f6]

(* Trace *)
let onProposalTrace label toString v1 v2 v3 = 
  let () = scream "[%s] -prop- current:%s asked:%s answer:%s\n" label (toString v1) (toString v2) (toString v3) in
  v3

let onCommitTrace label toString v1 v2 = 
  scream "[%s] -commit- old:%s new:%s\n" label (toString v1) (toString v2)


(* temps, mesure, reference, derivee, somme, controle *)
let pid =
  let facProp  = 0.5 in
  let facInteg = 0.3 in
  let facIntegE = 0.2 in
  let facIntegS = 0.8 in
  let facDer = 0.2 in
  let prop reference mesure =
    reference -. mesure
  in
  let integ reference mesure somme =
    (facIntegE *. (reference -. mesure)) +. (facIntegS *. somme)
  in
  let deriv mesure1 mesure2 diffTemps =
    (mesure2 -. mesure1) /. diffTemps
  in
  let ctrl propVal integVal deriVal =
    (facProp *. propVal) +. (facInteg *. integVal) +. (facDer *. deriVal)
  in
  return
    ~on_proposal:begin fun 
      (t1, m1, r1, d1, s1, c1)
      (t2, m2, r2, _d2, _s2, _c2)
      ->
      let (t3, m3, r3, d3, s3, c3) =
	let p = prop r2 m2 in
        let diffT = t2 -. t1 in
	if diffT = 0. then
	  let c2' = ctrl p s1 d1 in
	  (t2, m2, r2, d1, s1, c2')
        else
	  let s2' = integ r2 m2 s1 in
          let d2' = deriv m1 m2 diffT in
	  let c2' = ctrl p s2' d2' in
          (t2, m2, r2, d2', s2', c2')
      in
      onProposalTrace "pid" strf6
        (t1, m1, r1, d1, s1, c1)
        (t2, m2, r2, _d2, _s2, _c2)
        (t3, m3, r3, d3, s3, c3) 
      end
    ~on_commit:(onCommitTrace "pid" strf6)
    (0., 0., 0., 0., 0., 0.)
(* end pid *)

let setPid t m r = set pid (t, m, r, 0., 0., 0.)
let clock = Clock.make ~init:0 ~delay:1. ()
let g = group_pair
  ~on_proposal:begin fun (clock1, (t1, _, _, _, _, _)) (clock2, (t2, m2, r2, s2, d2, c2)) ->
    let (clock3, (t3, m3, r3, s3, d3, c3)) = (clock2, ((float_of_int clock2), m2, r2, s2, d2, c2)) in
    let () = ignore(onProposalTrace "clockTime" str2if (clock1, t1) (clock2, t2) (clock3, t3)) in
    (clock3, (t3, m3, r3, s3, d3, c3)) end
  ~on_commit:begin fun (clock1, (t1, _, _, _, _, _)) (clock2, (t2, _, _, _, _, _)) ->
    onCommitTrace "clockTime" str2if (clock1, t1) (clock2, t2) end
  clock
  pid
