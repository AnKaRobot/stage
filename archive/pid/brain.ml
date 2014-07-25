(* Print functions *)

(* Thread efficient printf *)
let scream fmt = Printf.kfprintf flush stdout fmt

(* convert to string *)
let s x = match x with
| None -> "None"  
| Some n -> try "Some " ^ string_of_int n
            with _ -> "???"

(* trace for resistances evolutions *)
let intProposal name x1 x2 x3 =
  scream "[%s] -proposal- current : %s ; asked %s ; answer : %s\n" 
    name (s x1) (s x2) (s x3)

let intCommit name x1 x2 =
  scream "[%s] -commit- current : %s ; new : %s\n" name (s x1) (s x2)

let intXintProposal name x1 y1 x2 y2 x3 y3 =
  scream "[%s] -proposal- current : (%s, %s) ; asked : (%s, %s) ; answser : (%s, %s)\n"
    name (s x1) (s y1) (s x2) (s y2) (s x3) (s y3)

let intXintCommit name x1 y1 x2 y2 =
  scream "[%s] -commit- current : (%s, %s) ; new : (%s, %s)\n"
    name (s x1) (s y1) (s x2) (s y2)  

(* Just for debugging: *)
let () = begin
  Printexc.record_backtrace true;
  Ocamlbricks_log.enable ();
  end

(* End Print functions *)


(* Network Communication module *)

(* using Unix Domain Sockets *)
module UDSCom = struct

(* one-type-message server *)
let listener ~socketfile ~receive ~unpack ~witness =
  let protocol =
    let () = scream "(UDSCom.listener) %s : -init-\n" socketfile in
    let rec loop channel =
      let msg = receive channel in
      let msgUnpacked, result = unpack msg in
      let () = match result with
      | true ->
          Cortex.set witness msgUnpacked;
          scream "(UDSCom.listener) %s : accepted then received : %s\n" 
                 socketfile msg
      | false -> scream "(UDSCom.listener) %s rejected then no received : %s\n" 
                        socketfile msg
      in
      loop channel
    in
    loop
  in
  Network.stream_unix_server
    ~socketfile
    ~protocol
    ~no_fork:() (* don't duplicate resistances *)
    ()

(* one-type-message client (several msgs by client) *)
let sender ~socketfile ~send ~pack ~orderer =
  let protocol =
    let () = scream "(UDSCom.sender) %s : -init-\n" socketfile in
    let rec loop channel =
      (* Passive waiting *)
      let msg = Cortex_lib.Channel.receive orderer in 
      let msgPacked, result = pack msg in
      let () = match result with
      | true ->
          send channel msgPacked;
          scream "(UDSCom.sender) %s : accepted then sent : %s\n" 
                 socketfile msgPacked
      | false -> scream "(UDSCom.sender) %s : rejected then no sent (no string available)\n" socketfile
      in
      loop channel
    in
    loop
  in (* isolate in a thread to not block main thread *)
  Thread.create ( 
    Network.stream_unix_client
      ~socketfile
      ~protocol )
  ()

let receiveLine channel = channel#input_line ()

let sendLine channel msg = channel#output_line msg

let unpackInt msg =
  try (int_of_string msg), true
  with Failure "int_of_string" -> 0, false

let packInt msg =
  try (string_of_int msg), true
  with Failure "string_of_int" -> "", false

let unpackIntOption msg = match msg with
| "N" -> None, true
| _ -> let x, isValid = unpackInt msg in Some x, isValid

let server ~socketfile ~protocol =
  Network.stream_unix_server
    ~socketfile
    ~protocol
    ~no_fork:()
    ()

end (* module UDSCom *)


(* To send messages from a simple resistance to a Channel resistance *)
(* Because Channels are not built for unguarded "set" or "propose" *)
(* There is a method "send" built for them *)
module Transmission = struct

let transmission ~resistance:r ~channel:ch ~convert =
  let order v = 
    let v2, isValid = convert v in
    if isValid then ignore(Cortex_lib.Channel.send ch v2) else ()
  in
  let rec loop prev =
    let next = Cortex.get ~guard:(fun v -> v <> prev) r in
    let () = order next in
    loop next
  in
  let init = Cortex.get r in
  let () = order init in
  Thread.create loop init

end (* module Transmission *)

(* Range functions *)
let notAccomodatingRange min max old next =
    if next >= min && next <= max
    then next
    else old

let accomodatingRange min max old next = match next with
| next when next > max -> max
| next when next < min -> min
| _ -> next

let optionAccomodatingRange min max old next = match next with
| None                -> None
| Some n when n > max -> Some max
| Some n when n < min -> Some min

let optionNotAccomodatingRange min max old next = match next with
| None                -> None
| Some n -> if n >= min && n <= max then Some n else old

(* End range functions *)


(* Resistances definitions *)

let speedWitness = 
  let range = optionNotAccomodatingRange (-300) 300 in
  Cortex.return
    ~on_proposal:begin fun x1 x2 ->
      let x3 = range x1 x2 in
      let () = intProposal "speedWitness" x1 x2 x3 in
      x3
      end
    ~on_commit:(fun x1 x2 -> intCommit "speedWitness" x1 x2)
    None

let speedOrderer =
  let range = optionAccomodatingRange (-300) 300 in
  Cortex_lib.Channel.return
    ~on_proposal:begin fun x1 x2 ->
      let x3 = range x1 x2 in
      let () = intProposal "speedOrderer" x1 x2 x3 in
      x3
      end
    ~on_commit:(fun x1 x2 -> intCommit "speedOrderer" x1 x2)
    ()

let speedListener = UDSCom.listener
  ~socketfile:"/tmp/speedWitness"
  ~receive:UDSCom.receiveLine
  ~unpack:UDSCom.unpackIntOption
  ~witness:speedWitness

let speedSender = UDSCom.sender
  ~socketfile:"/tmp/speedOrderer"
  ~send:UDSCom.sendLine
  ~pack:UDSCom.packInt
  ~orderer:speedOrderer

let witnessToOrder = 
  let convert intOpt = match intOpt with
  | None -> 0, false
  | Some n -> n, true
  in
  Transmission.transmission
    ~resistance:speedWitness
    ~channel:speedOrderer
    ~convert

(* End Resistances definitions *)

(* Loop to not ending the main thread immediatly *)
let rec loop i = 
  let v = Cortex.get speedWitness in
  if v = Some 42 || i < 0 then () else begin
    scream "(time loop) still not : %s\n" (s v);
    Thread.delay 1.0;
    loop (i - 1)
    end
;;
loop 20