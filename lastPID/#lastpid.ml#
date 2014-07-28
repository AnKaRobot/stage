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

let str5 x1 x2 x3 x4 x5 = "(" ^ x1 ^ ", " ^ x2 ^ ", " ^ x3 ^ ", " ^ x4 ^ ", " ^ x5 ^ ")"

let strf6 (f1, f2, f3, f4, f5, f6) = strlf [f1; f2; f3; f4; f5; f6]

let str3 x1 x2 x3 = "(" ^ x1 ^ ", " ^ x2 ^ ", " ^ x3 ^ ")"
let str3iff (i, f1, f2) = str3 (stri i) (strf f1) (strf f2)

let str4 x1 x2 x3 x4 = "(" ^ x1 ^ ", " ^ x2 ^ ", " ^ x3 ^ ", " ^ x4 ^ ")"
let stri_f_f_4f (i, f1, f2, (f3, f4, f5, f6)) = 
  str4 (stri i) (strf f1) (strf f2) (strlf [f3; f4; f5; f6])

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

let returnTrace ?hide ?equality ?onProposal ?onCommit ?sep name toStr init = 
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
      if hide = None then onProposalTrace ~sep name toStr v1 v2 v3
      else v3 end
    ~on_commit:begin fun v1 v2 ->
      let () = match onCommit with
      | None -> ()
      | Some f -> f v1 v2
      in
      if hide = None then onCommitTrace ~sep name toStr v1 v2 end
    init

let group_pairTrace 
  ?onProposal ?onCommit ?sep name toStr res1 res2 = 
  let sep = match sep with
  | None -> "\n          " ^ (StringExtra.map (fun x -> ' ') name)
  | Some s -> s
  in
  group_pair
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

let group_tripleTrace 
  ?onProposal ?onCommit ?sep name toStr res1 res2 res3 = 
  let sep = match sep with
  | None -> "\n          " ^ (StringExtra.map (fun x -> ' ') name)
  | Some s -> s
  in
  group_triple
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


let group_quintupleTrace 
  ?hide ?onProposal ?onCommit ?sep name toStr res1 res2 res3 res4 res5 = 
  let sep = match sep with
  | None -> "\n          " ^ (StringExtra.map (fun x -> ' ') name)
  | Some s -> s
  in
  group_quintuple
    ~on_proposal:begin fun v1 v2 ->
      let v3 = match onProposal with
      | None -> v2
      | Some f -> f v1 v2
      in
      if hide = None then onProposalTrace ~sep name toStr v1 v2 v3
      else v3 end
    ~on_commit:begin fun v1 v2 ->
      let () = match onCommit with
      | None -> ()
      | Some f -> f v1 v2
      in
      if hide = None then onCommitTrace ~sep name toStr v1 v2 end
    res1
    res2
    res3
    res4
    res5

(* ----------------------------------------------------- *)

(* let time = Clock.make ~delay:5. () *)
let time = return 0
let factors = return (0.5, 0.2, 0.3) (* p, i, d *)
let mesure = return 0.
let reference = return 0.
let deriveeTemporal = return 0.
let sumTemporal = return 0.

let prop = view
  (fun (m, r) -> r -. m)
  (group_pair mesure reference)

let strSpecial (i1, i2, f1, f2, f3) = str5 (stri i1) (stri i2) (strf f1) (strf f2) (strf f3)

let deriveeTemporalCalcul = group_quintupleTrace
  ~onProposal:begin fun (t0, tp0, m0, mp0, d0) (t1, tp1, m1, mp1, d1) ->
    let dt = t1 - tp1 in
    if dt = 0
    then (t1, tp1, m1, mp1, d1)
    else 
      let d2 = (m1 -. mp1) /. (float_of_int dt) in
      (t1, t1, m1, m1, d2)
    end
  "deriveeTemporalCalcul" strSpecial
  time (return 0) mesure (return 0.) deriveeTemporal

let strSpecial2 (i1, i2, f1, f2) = str4 (stri i1) (stri i2) (strf f1) (strf f2)

let sumTemporalCalcul = group_quadrupleTrace
  ~onProposal:begin fun (t0, tp0, m0, s0) (t1, tp1, m1, s1) ->
  let dt = t1 - tp1 in 
  if dt = 0
  then (t1, tp1, m1, s1)
  else 
    let s2 = s1 +. (m1 *. (float_of_int dt)) in
    (t1, t1, m1, s2)
  end
  "sumTemporalCalcul" strSpecial2
  time (return 0) mesure sumTemporal

let pid = view
  (fun (p, i, d, (pF, iF, dF)) -> (p *. pF) +. (i *. iF)  +. (d *. dF) )
  (group_quadruple prop sumTemporal deriveeTemporal factors)
