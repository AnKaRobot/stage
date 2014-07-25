(* Print functions *)

let scream fmt = Printf.kfprintf flush stdout fmt

let s x = match x with
| None -> "N"
| Some v -> string_of_int v

let intProposal name x1 x2 x3 =
  scream "[%s] -proposal- current : %s ; asked %s ; answer : %s\n" 
    name (s x1) (s x2) (s x3)

let intCommit name x1 x2 =
  scream "[%s] -commit- current : %s ; new : %s\n" name (s x1) (s x2)

let intXintProposal name x1 y1 x2 y2 x3 y3=
  scream "[%s] -proposal- current : (%s, %s) ; asked : (%s, %s) ; answser : (%s, %s)\n"
    name (s x1) (s y1) (s x2) (s y2) (s x3) (s y3)

let intXintCommit name x1 y1 x2 y2 =
  scream "[%s] -commit- current : (%s, %s) ; new : (%s, %s)\n"
    name (s x1) (s y1) (s x2) (s y2)  

(* End Print functions *)


(* Resistances definitions *)

let a = Cortex.return
  ~on_proposal:begin fun a1 a2 ->
    let () =  intProposal "a" a1 a2 a2 in
    a2
    end
  ~on_commit:(fun a1 a2 -> intCommit "a" a1 a2)
  None

let b = Cortex.return
  ~on_proposal:begin fun b1 b2 ->
    let () =  intProposal "b" b1 b2 b2 in
    b2
    end
  ~on_commit:(fun b1 b2 -> intCommit "b" b1 b2)
  None

let ab = Cortex.group_pair
  ~on_proposal:begin fun (a1, b1) (a2, b2) ->
    let (a3, b3) = 
      match (a2, b2) with
      | (None, None) -> (a2, b2)
      | (Some v, None) -> begin match a1 with
                          | Some w when w <> v -> (a2, a1)
                          | _ -> (a2, b2)
                          end
      | (None, Some v) -> begin match b1 with
                          | Some w when w <> v -> (b1, b2)
                          | _ -> (a2, b2)
                          end
      | (Some v, Some x) -> begin match (a1, b1) with
                          | (Some w, Some y) when w <> v && x = y -> (a2, a1)
                          | (Some w, Some y) when w = v && x <> y -> (b1, b2)
                          | (Some w, Some y) when w <> v && x <> y -> (a2, b2)
                          | (Some w, Some y) when w = v && x = y -> (a2, b2)
                          | (Some w, None) -> (a2, b2)
                          | (None, Some y) -> (a2, b2)
                          | (None, None) -> (a2, b2)
                          | _ -> (a1, b1)
                          end
    in
    let () = intXintProposal "ab" a1 b1 a2 b2 a3 b3 in
    (a3, b3)
    end
  ~on_commit:(fun (a1, b1) (a2, b2) -> intXintCommit "ab" a1 b1 a2 b2)
  a
  b

let c = Cortex.return
  ~on_proposal:begin fun b1 b2 ->
    let () =  intProposal "c" b1 b2 b2 in
    b2
    end
  ~on_commit:(fun b1 b2 -> intCommit "c" b1 b2)
  None


(* End Resistances definitions *)

