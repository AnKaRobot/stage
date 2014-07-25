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

let strf3 (f1, f2, f3) = strlf [f1; f2; f3]
let strf6 (f1, f2, f3, f4, f5, f6) = strlf [f1; f2; f3; f4; f5; f6]

let str4 x1 x2 x3 x4 = "(" ^ x1 ^ ", " ^ x2 ^ ", " ^ x3 ^ ", " ^ x4 ^ ")"
let stri_f_f_3f (i, f1, f2, (f3, f4, f5)) = 
  str4 (stri i) (strf f1) (strf f2) (strf3 (f3, f4, f5))

(* Trace *)
let onProposalTrace ?(sep=" ") label toString v1 v2 v3 = 
  let () = scream 
     "[%s] -prop- current:%s%sasked:  %s%sanswer: %s\n" 
     label (toString v1) sep (toString v2) sep (toString v3) 
  in
  v3

let onCommitTrace ?(sep=" ") label toString v1 v2 = 
  scream 
    "[%s] -commit- old:%s%s  new:%s\n" 
    label (toString v1) sep (toString v2)

let returnTrace ?equality ?onProposal ?onCommit ?sep name toStr init = 
  let sep = match sep with
  | None -> "\n          " ^ (StringExtra.map (fun x -> ' ') name)
  | Some s -> s
  in
  return
    ?equality
    ~on_proposal:begin fun v1 v2 ->
      let v3 = match onProposal with
      | None -> v2
      | Some f -> f v1 v2
      in
      onProposalTrace ~sep name toStr v1 v2 v3 end
    ~on_commit:begin fun v1 v2 ->
      let () = match onCommit with
      | None -> ()
      | Some f -> f v1 v2
      in
      onCommitTrace ~sep name toStr v1 v2 end
    init

let group_quadrupleTrace 
  ?onProposal ?onCommit ?sep name toStr res1 res2 res3 res4 = 
  let sep = match sep with
  | None -> "\n          " ^ (StringExtra.map (fun x -> ' ') name)
  | Some s -> s
  in
  group_quadruple
    ~on_proposal:begin fun v1 v2 ->
      let v3 = match onProposal with
      | None -> v2
      | Some f -> f v1 v2
      in
      onProposalTrace ~sep name toStr v1 v2 v3 end
    ~on_commit:begin fun v1 v2 ->
      let () = match onCommit with
      | None -> ()
      | Some f -> f v1 v2
      in
      onCommitTrace ~sep name toStr v1 v2 end
    res1
    res2
    res3
    res4

(* ------------------------------- *)


(* let time = Clock.make ~delay:0.5 () *)
let time = return 0
(* On l'utilise en "manuel" pour faire des tests,
   sinon le thread de clock rend les tests insupportables *)

let mesure = return 0.

let reference = return 0.

let pidLocal = return (0., 0., 0.) (* sum, deriv, control *)

let pid = 
  let facProp   = 0.5 in
  let facInteg  = 0.3 in
  let facIntegE = 0.2 in
  let facIntegS = 0.8 in
  let facDer    = 0.2 in
  let prop r m = r -. m in
  let integ r m s = (facIntegE *. (prop r m)) +. (facIntegS *. s) in
  let deriv m1 m2 dt = (m2 -. m1) /. dt in
  let ctrl p i d = (facProp *. p) +. (facInteg *. i) +. (facDer *. d) in
  let proposal (t1, m1, r1, (s1, d1, c1)) (t2, m2, r2, (s2, d2, c2)) =
    let p = prop r2 m2 in
    let dt = (float_of_int (t2 - t1)) in
    if dt = 0. then
      let c3 = ctrl p s1 d1 in
      (t2, m2, r2, (d1, s1, c3))
    else
      let s3 = integ r2 m2 s1 in
      let d3 = deriv m1 m2 dt in
      let c3 = ctrl p s3 d3 in
      (t2, m2, r2, (d3, s3, c3))
  in
  group_quadrupleTrace
    ~onProposal:proposal
    "pid"
    stri_f_f_3f
    time
    mesure
    reference
    pidLocal

