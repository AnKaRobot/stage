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
    let () = intProposal "x" x1 x2 x2 in
    x2
    end
  ~on_commit:(fun x1 x2 -> intCommit "x" x1 x2)
  0

let y = Cortex.return
  ~on_proposal:begin fun y1 y2 ->
    let () = intProposal "y" y1 y2 y2 in
    y2
    end
  ~on_commit:(fun y1 y2 -> intCommit "y" y1 y2)
  0

let z = Cortex.group_pair
  ~on_proposal:begin fun (x1, y1) (x2, y2) ->
    let x3, y3 = if (x2 + y2) mod 2 = 0 
                 then (x2, y2)
                 else (x1, y1)
    in
    let () = intXintProposal "z" x1 y1 x2 y2 x3 y3 in
    (x3, y3)
    end
  ~on_commit:begin fun (x1, y1) (x2, y2) -> 
    intXintCommit "z" x1 y1 x2 y2
    end
  x
  y

(* End Resistances definitions *)

;;

Cortex.get x ;;
Cortex.get y ;;
Cortex.get z ;;

scream "------\n" ;;
Cortex.set x 1 ;;
Cortex.get x ;;
Cortex.get y ;;
Cortex.get z ;;
