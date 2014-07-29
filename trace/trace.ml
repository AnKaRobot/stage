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

(* fonctions basiques de trace *)
let onProposalT ?large label printer values0 values1 values2 =
  let startMsg = match large with
  | None -> "[" ^ label ^ "] proposal :" 
  | _ -> "====================\n[" ^ label ^ "] proposal :"
  in
  let btwin = match large with
  | None -> " "
  | _ -> "\n | "
  in
  let currentValues = btwin ^ "current  : " ^ printer values0 in (* printer is : (formatTuple myCurrying myFormat) *)
  let proposedValues = btwin ^ "proposed : " ^ printer values1 in
  let acceptedValues = match values2 with 
  | None -> ""
  | Some values -> btwin ^ "accepted : " ^ printer values in
  scream "%s%s%s%s\n" startMsg currentValues proposedValues acceptedValues

let onCommitT ?large label printer values0 values2 =
  let startMsg = match large with
  | None -> "[" ^ label ^ "] commit :  " 
  | _ -> "====================\n[" ^ label ^ "] commit :"
  in
  let btwin = match large with
  | None -> " "
  | _ -> "\n | "
  in
  let oldValues = btwin ^ "old : " ^ printer values0 in
  let newValues = btwin ^ "new : " ^ printer values2 in
  scream "%s%s%s\n" startMsg oldValues newValues

let equalityT ?large label printer valuesA valuesB result =
  let startMsg = match large with
  | None -> "[" ^ label ^ "] equality :"
  | _ -> "====================\n[" ^ label ^ "] equality :"
  in
  let btwin = match large with
  | None -> " "
  | _ -> "\n | "
  in
  let valuesLeft = btwin ^ printer valuesA ^ " ="  in
  let valuesRight = btwin ^ printer valuesB in
  let resultMsg = match result with
  | None -> ""
  | Some true -> btwin ^ "-> True"
  | Some false -> btwin ^ "-> False"
  in
  scream "%s%s%s%s\n" startMsg valuesLeft valuesRight resultMsg

(* fonctions de trace qui s'ajoutent à une fonction définie par l'utilisateur *)
let onProposalTWrap ?large ?noTraceProposal ?noAnswers ?on_proposal label printer values0 values1 =
    let values2 = match on_proposal with
    | None -> values1 
    | Some f -> f values0 values1
    in
    let someValues2 = match noAnswers with
    | None -> Some values2
    | _ -> None
    in
    let () = match noTraceProposal with
    | None -> onProposalT ?large label printer values0 values1 someValues2 
    | _ -> () 
    in
    values2

let onCommitTWrap ?large ?noTraceCommit ?on_commit label printer values0 values2 =
  let () = match on_commit with
  | None -> ()
  | Some f -> f values0 values2 
  in
  match noTraceCommit with
  | None -> onCommitT ?large label printer values0 values2
  | _ -> ()

let equalityTWrap ?large ?traceEquality ?noAnswers ?equality label printer valuesA valuesB = 
  let res = match equality with
  | None -> (valuesA = valuesB)
  | Some f -> f valuesA valuesB 
  in
  let someRes = match noAnswers with
  | None -> Some res
  | _ -> None
  in
  let () = match traceEquality with
  | None -> ()
  | _-> equalityT ?large label printer valuesA valuesB someRes
  in
  res

(* fonctions haut niveau qui retournent une résistance avec des fonctions de trace *)
let returnT ?equality ?on_proposal ?on_commit ?large ?noAnswers ?noTraceProposal ?noTraceCommit ?traceEquality label currying format initValue = 
  let printer = (formatTuple currying format) in
  Cortex.return 
    ~equality:(equalityTWrap ?large ?traceEquality ?noAnswers ?equality label printer)
    ~on_proposal:(onProposalTWrap ?large ?noTraceProposal ?noAnswers ?on_proposal label printer)
    ~on_commit:(onCommitTWrap ?large ?noTraceCommit ?on_commit label printer)
    initValue

let group_pairT ?equality ?on_proposal ?on_commit ?large ?noAnswers ?noTraceProposal ?noTraceCommit ?traceEquality label currying format initValue = 
  let printer = (formatTuple currying format) in
  Cortex.group_pair
    ~on_proposal:(onProposalTWrap ?large ?noTraceProposal ?noAnswers ?on_proposal label printer)
    ~on_commit:(onCommitTWrap ?large ?noTraceCommit ?on_commit label printer)
    initValue

let group_tripleT ?equality ?on_proposal ?on_commit ?large ?noAnswers ?noTraceProposal ?noTraceCommit ?traceEquality label currying format initValue = 
  let printer = (formatTuple currying format) in
  Cortex.group_triple
    ~on_proposal:(onProposalTWrap ?large ?noTraceProposal ?noAnswers ?on_proposal label printer)
    ~on_commit:(onCommitTWrap ?large ?noTraceCommit ?on_commit label printer)
    initValue

let group_tripleT ?equality ?on_proposal ?on_commit ?large ?noAnswers ?noTraceProposal ?noTraceCommit ?traceEquality label currying format initValue = 
  let printer = (formatTuple currying format) in
  Cortex.group_triple
    ~on_proposal:(onProposalTWrap ?large ?noTraceProposal ?noAnswers ?on_proposal label printer)
    ~on_commit:(onCommitTWrap ?large ?noTraceCommit ?on_commit label printer)
    initValue

let group_quadrupleT ?equality ?on_proposal ?on_commit ?large ?noAnswers ?noTraceProposal ?noTraceCommit ?traceEquality label currying format initValue = 
  let printer = (formatTuple currying format) in
  Cortex.group_quadruple
    ~on_proposal:(onProposalTWrap ?large ?noTraceProposal ?noAnswers ?on_proposal label printer)
    ~on_commit:(onCommitTWrap ?large ?noTraceCommit ?on_commit label printer)
    initValue

let group_quintupleT ?equality ?on_proposal ?on_commit ?large ?noAnswers ?noTraceProposal ?noTraceCommit ?traceEquality label currying format initValue = 
  let printer = (formatTuple currying format) in
  Cortex.group_quintuple
    ~on_proposal:(onProposalTWrap ?large ?noTraceProposal ?noAnswers ?on_proposal label printer)
    ~on_commit:(onCommitTWrap ?large ?noTraceCommit ?on_commit label printer)
    initValue

let viewT ?equality ?on_proposal ?on_commit ?large ?noAnswers ?noTraceProposal ?noTraceCommit ?traceEquality ?private_fellow 
	      convert label currying format initValue =
  let printer = (formatTuple currying format) in
  Cortex.view
    ~equality:(equalityTWrap ?large ?traceEquality ?noAnswers ?equality label printer)
    ~on_proposal:(onProposalTWrap ?large ?noTraceProposal ?noAnswers ?on_proposal label printer)
    ~on_commit:(onCommitTWrap ?large ?noTraceCommit ?on_commit label printer)
    ?private_fellow
    convert
    initValue

(* TODO : faire la doc de ce truc !!! *) 
 
end (* module Trace *)
