(* Print functions *)
let scream fmt = Printf.kfprintf flush stdout fmt

let intProposal name x1 x2 x3 =
  scream "[%s] -proposal- current : %d ; asked %d ; answer : %d\n" 
    name x1 x2 x3

let intCommit name x1 x2 =
  scream "[%s] -commit- current : %d ; new : %d\n" name x1 x2

let intXintProposal name x1 y1 x2 y2 x3 y3=
  scream "[%s] -proposal- current : (%d, %d) ; asked : (%d, %d) ; answser : (%d, %d)\n"
    name x1 y1 x2 y2 x3 y3

let intXintCommit name x1 y1 x2 y2 =
  scream "[%s] -commit- current : (%d, %d) ; new : (%d, %d)\n"
    name x1 y1 x2 y2  

(* End Print functions *)


(* Resistances definitions *)

let x = Cortex.return
  ~on_proposal:begin fun x1 x2 ->
    let () = intProposal "x" x1 x2 (x1 + 1) in
    (x1 + 1)
    end
  ~on_commit:(fun x1 x2 -> intCommit "x" x1 x2)
  0
;;

Cortex.on_proposal_append x
  (fun x1 x2 ->
    let () = intProposal "x'" x1 x2 (x2 - 1) in (x2 - 1))

(* End Resistances definitions *)

;;
