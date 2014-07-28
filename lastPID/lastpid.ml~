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

(* ----------------------------------------------------- *)

let sum = returnTrace
  ~onProposal:begin fun (s0, b0) (s1, b1) -> match b1 with
    | true -> (s0 + s1, false)
    | _    -> (s1, b1)
    end
  "sum" str2ib (0, false)

