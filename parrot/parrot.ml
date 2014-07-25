(* Print functions *)

(* Thread efficient printf *)
let scream fmt = Printf.kfprintf flush stdout fmt

(* Convertion functions *)
let stri i = string_of_int i
let strb b = if b then "T" else "F"
let stri' i' = match i' with
| None -> "None"
| Some i -> "Some " ^ (stri i)

let str2 l r = "(" ^ l ^ ", " ^ r ^ ")"

let strib (i, b) = str2 (stri i) (strb b)
let strii (i1, i2) = str2 (stri i1) (stri i2)
let strii' (i, i') = str2 (stri i) (stri' i')

let strii'_i ((i1, i'), i2) = str2 (strii' (i1, i')) (stri i2)

(* Just for debugging: *)
(* let () = begin
  Printexc.record_backtrace true;
  Ocamlbricks_log.enable ();
  end *)

(* End Print functions *)


open Cortex
open Cortex_lib


(* Resistances functions *)

(* Parrot *)
let onProposalParrot (v1, i1) (v2, i2) = match i1 with
| None -> (v2, Some 0)
| Some x -> (v2, Some (x + 1))

let returnP ?equalityAppend ?onProposalAppend ?onCommitAppend initValue = return
  ~equality:begin fun (v1, i1) (v2, i2) -> match equalityAppend with (* Not sure if working *)
    | None -> (v1, i1) = (v2, i2)
    | Some f -> f v1 v2 && i1 = i2
    end
  ~on_proposal:begin fun (v1, i1) (v2, i2) ->
    let (v3, i3) = onProposalParrot (v1, i1) (v2, i2) in
    let v3' = match onProposalAppend with
    | None -> v3
    | Some f -> f v1 v3
    in
    (v3', i3) end
  ~on_commit:begin fun (v1, i1) (v2, i2) -> 
    match onCommitAppend with
    | None -> ()
    | Some f -> f v1 v2
    end
  (initValue, Some 0)

let setP ?guard parrot value = set ?guard parrot (value, None)
(* TODO : guard for setP and getP *)
let getP ?guard parrot = 
  let v, i = get ?guard parrot in
  v

(* Trace *)
let onProposalTrace label toString v1 v2 v3 = 
  let () = scream "[%s] -prop- current:%s asked:%s answer:%s\n" label (toString v1) (toString v2) (toString v3) in
  v3

let onCommitTrace label toString v1 v2 = 
  scream "[%s] -commit- old:%s new:%s\n" label (toString v1) (toString v2)

(* End Resistances functions *)


(* Resistances definitions *)

(* Example Simple *)
let badParrot = return
  ~on_commit:(fun v1 v2 -> scream "<badParrot> : %d !!!" v2)
  0

let parrot = return
  ~on_proposal:begin fun (v1, i1) (v2, i2) ->
    match i1 with
    | None -> (v2, Some 0)
    | Some x -> (v2, Some (x + 1)) 
    end
  ~on_commit:(fun (v1, i1) (v2, i2) -> scream "<parrot> : %d !!!" v2)
  (0, Some 0)

(* Middle-level Usage *)
let niceParrot = return 
  ~on_proposal:begin fun p1 p2 ->
    let p3 = onProposalParrot p1 p2 in
    onProposalTrace "niceParrot" strii' p1 p2 p3 end
  ~on_commit:(onCommitTrace "niceParrot" strii')
  (0, Some 0)

(* High-level Usage *)
let superParrot = returnP
  ~onProposalAppend:begin fun v1 v2 ->
    let v3 = v2 in
    onProposalTrace "superParrot" stri v1 v2 v3 end
  ~onCommitAppend:(onCommitTrace "superParrot" stri)
  0

(* Parrot Combinaison *)
let xParrot = returnP
  ~onProposalAppend:begin fun v1 v2 ->
    let v3 = v2 in
    onProposalTrace "xParrot" stri v1 v2 v3 end
  ~onCommitAppend:(onCommitTrace "xParrot" stri)
  0
let y = return 
  ~on_proposal:begin fun v1 v2 ->
   let v3 = v2 in
   onProposalTrace "y" stri v1 v2 v3 end
  ~on_commit:(onCommitTrace "y" stri)
  0
let g = group_pair
  ~on_proposal:begin fun ((a1, i1), b1) ((a2, i2), b2) ->
    let ((a3, i3), b3) = match (a2, b2) with
    | (a2, b2) when a2 + b2 < 10 -> ((a2, i2), b2)
    | _ -> ((a1, i2), b1) (* i2 because i1 would cause infinite looping *)
    in
    let () = ignore(onProposalTrace "g" strii (a1, b1) (a2, b2) (a3, b3) ) in
    ((a3, i3), b3) end
  ~on_commit:begin fun ((a1, i1), b1) ((a2, i2), b2) -> onCommitTrace "g" strii (a1, b1) (a2, b2) end
  xParrot
  y

(* End Resistances definitions *)

;;

print_endline 
"
---------------------------
Design Pattern : the Parrot
---------------------------

* launch this code in utop so you can test the resistances of the examples *

---[Why]---
When you do : 'set foo 42;;'
if the value of 'foo' is already '42', nothing happens.
No 'on_proposal' or 'on_commit' is called.
But sometimes, you do want to activate something, even if the value is the same.


---[Example single]---
A parrot who repeat everything :
'''
let badParrot = return
  ~on_commit:(fun v1 v2 -> scream \"<badParrot> : %d !!!\" v2)
  0
;;
set badParrot 10 ;;
set badParrot 10 ;;
'''
This is going to fail. Because propose '10' to 'badParrot' which already hold '10' will not call 'on_commit'.
If you want the 'on_proposal' to be called, you must set a value different from the current.
If you want the 'on_commit' to be called, 'on_proposal' must propose a value different from the current.

Consider the following structure :
'''
let parrot = return
  ~on_proposal:begin fun (v1, i1) (v2, i2) ->
    match i1 with
    | None -> (v2, Some 0)
    | Some x -> (v2, Some (x + 1)) 
    end
  ~on_commit:(fun (v1, i1) (v2, i2) -> scream \"<parrot> : %d !!!\" v2)
  (0, Some 0)
'''

---[Middle-level Usage]---
Here is an example of a parrot defined with the help of on_proposal functions (provided in the source code)
'''
let niceParrot = return 
  ~on_proposal:begin fun p1 p2 ->
    let p3 = onProposalParrot p1 p2 in
    onProposalTrace \"niceParrot\" strii' p1 p2 p3 end
  ~on_commit:(onCommitTrace \"niceParrot\" strii')
  (0, Some 0)
;;
set niceParrot (10, None) ;;
set niceParrot (10, None) ;;
get niceParrot ;;
'''

---[High-level Usage]---
Here is another example built with higher level functions to abstract the parrot
'''
let superParrot = returnP
  ~onProposalAppend:begin fun v1 v2 ->
    let v3 = v2 in
    onProposalTrace \"superParrot\" stri v1 v2 v3 end
  ~onCommitAppend:(onCommitTrace \"superParrot\" stri)
  0
;;
set superParrot 10 ;;
set superParrot 10 ;;
get superParrot ;;
'''


---[Parrot Combinaison]---
Sum of a parrot and an int that must be < 10
'''
let xParrot = returnP
  ~onProposalAppend:begin fun v1 v2 ->
    let v3 = v2 in
    onProposalTrace \"xParrot\" stri v1 v2 v3 end
  ~onCommitAppend:(onCommitTrace \"xParrot\" stri)
  0
let y = return 
  ~on_proposal:begin fun v1 v2 ->
   let v3 = v2 in
   onProposalTrace \"y\" stri v1 v2 v3 end
  ~on_commit:(onCommitTrace \"y\" stri)
  0
let g = group_pair
  ~on_proposal:begin fun ((a1, i1), b1) ((a2, i2), b2) ->
    let ((a3, i3), b3) = match (a2, b2) with
    | (a2, b2) when a2 + b2 < 10 -> ((a2, i2), b2)
    | _ -> ((a1, i2), b1) (* i2 because i1 would cause infinite looping *)
    in
    let () = ignore(onProposalTrace \"g\" strii (a1, b1) (a2, b2) (a3, b3) ) in
    ((a3, i3), b3) end
  ~on_commit:begin fun ((a1, i1), b1) ((a2, i2), b2) -> onCommitTrace \"g\" strii (a1, b1) (a2, b2) end
  xParrot
  y
'''
Try 'setP xParrot 11 ;;' : you can observe that 'g' refuse the update, and 'xParrot' too.
But if you do 'set y 11 ;;' : 'g' will refuse the value, but 'y' will be '11'. Because 'y' is not a parrot !

"
