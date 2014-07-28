open Cortex
open Cortex_lib

let scream fmt = Printf.kfprintf flush stdout fmt

module Trace = struct

let formatTuple currying format tuple = currying (Printf.sprintf format) tuple

let currying1 f a = f a
let currying2 f (a, b) = f a b
let currying3 f (a, b, c) = f a b c
let currying4 f (a, b, c, d) = f a b c d
let currying5 f (a, b, c, d, e) = f a b c d e

let onProposalT ?large label format currying values0 values1 values2 =
  let startMsg = match large with
  | None -> "[" ^ label ^ "] proposal :" 
  | _ -> "====================\n[" ^ label ^ "] proposal :"
  in
  let btwin = match large with
  | None -> " "
  | _ -> "\n | "
  in
  let currentValues = btwin ^ "current  : " ^ (formatTuple currying format values0) in
  let proposedValues = btwin ^ "proposed : " ^ (formatTuple currying format values1) in
  let acceptedValues = match values2 with 
  | None -> ""
  | Some values -> btwin ^ "accepted : " ^ (formatTuple currying format values) in
  scream "%s%s%s%s\n" startMsg currentValues proposedValues acceptedValues

let onCommitT ?large label format currying values0 values2 =
  let startMsg = match large with
  | None -> "[" ^ label ^ "] commit :  " 
  | _ -> "====================\n[" ^ label ^ "] commit :"
  in
  let btwin = match large with
  | None -> " "
  | _ -> "\n | "
  in
  let oldValues = btwin ^ "old : " ^ (formatTuple currying format values0) in
  let newValues = btwin ^ "new : " ^ (formatTuple currying format values2) in
  scream "%s%s%s\n" startMsg oldValues newValues

let equalityT ?large label format currying valuesA valuesB result =
  let startMsg = match large with
  | None -> "[" ^ label ^ "] equality :"
  | _ -> "====================\n[" ^ label ^ "] equality :"
  in
  let btwin = match large with
  | None -> " "
  | _ -> "\n | "
  in
  let valuesLeft = btwin ^ (formatTuple currying format valuesA) ^ " ="  in
  let valuesRight = btwin ^ (formatTuple currying format valuesB) in
  let resultMsg = match result with
  | None -> ""
  | Some true -> btwin ^ "-> True"
  | Some false -> btwin ^ "-> False"
  in
  scream "%s%s%s%s\n" startMsg valuesLeft valuesRight resultMsg


let returnT ?equality ?on_proposal ?on_commit ?large ?noAnswers ?noTraceProposal ?noTraceCommit ?traceEquality label format currying =  
  let matchNoAnswers thing = match noAnswers with
  | None -> Some thing
  | _ -> None
  in
  let on_proposal values0 values1 =
    let values2 = match on_proposal with
    | None -> values1 
    | Some f -> f values0 values1
    in
    let () = match noTraceProposal with
    | None -> onProposalT ?large label format currying values0 values1 (matchNoAnswers values2) 
    | _ -> () 
    in
    values2
  in
  let on_commit values0 values2 =
    let () = match on_commit with
    | None -> ()
    | Some f -> f values0 values2 
    in
    match noTraceCommit with
    | None -> onCommitT ?large label format currying values0 values2
    | _ -> ()
  in
  let equality valuesA valuesB = 
    let res = match equality with
    | None -> valuesA = valuesB 
    | Some f -> f valuesA valuesB 
    in
    let () = match traceEquality with
    | None -> ()
    | _-> equalityT ?large label format currying valuesA valuesB (matchNoAnswers res)
    in
    res
  in
  Cortex.return ~equality ~on_proposal ~on_commit

(*
let groupT ?on_proposal ?on_commit ?large ?noAnswers ?noTraceProposal ?noTraceCommit label format currying groupNumber =
  let matchNoAnswers thing = match noAnswers with
  | None -> Some thing
  | _ -> None
  in
  let on_proposal values0 values1 =
    let values2 = match on_proposal with
    | None -> values1 
    | Some f -> f values0 values1
    in
    let () = match noTraceProposal with
    | None -> onProposalT ?large label format currying values0 values1 (matchNoAnswers values2) 
    | _ -> () 
    in
    values2
  in
  let on_commit values0 values2 =
    let () = match on_commit with
    | None -> ()
    | Some f -> f values0 values2 
    in
    match noTraceCommit with
    | None -> onCommitT ?large label format currying values0 values2
    | _ -> ()
  in
  group_pair ~on_proposal ~on_commit 
*)

(* TODO : exporter les fonctions onProposalTAppend etc pour pouvoir construire facilement des nouvelles fonctions comme group view etc *)
(* TODO : faire la doc de ce truc !!! *) 
 
end (* module Trace *)
