(* Thread-efficient printf *)
let scream fmt = Printf.kfprintf flush stdout fmt

(* Just for debugging: *)
let () = begin
  Printexc.record_backtrace true;
  Ocamlbricks_log.enable ();
  end

module UDSCom = struct

(* low level listener structure *)
let listener ~socketfile ~receive ~unpack ~witness =
  let protocol =
    let rec loop channel =
      let msg = receive channel in
      let msgUnpacked, result = unpack msg in
      let () = match result with
      | true ->
          Cortex.set witness msgUnpacked;
          scream "accepted then received : %s\n" msg
      | false -> scream "rejected then no received : %s\n" msg
      in
      loop channel
    in
    loop
  in
  Network.stream_unix_server
    ~socketfile
    ~protocol
    ~no_fork:()
    ()

(* low level sender structure *)
let sender ~socketfile ~send ~pack ~orderer =
  let protocol =
    let rec loop channel =
      (* Passive waiting *)
      let msg = Cortex_lib.Channel.receive orderer in 
      let msgPacked, result = pack msg in
      let () = match result with
      | true ->
          send channel msgPacked;
          scream "accepted then sent : %s\n" msgPacked
      | false -> scream "rejected then no sent (no string available)\n"
      in
      loop channel
in
    loop
  in
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


let server ~socketfile ~protocol =
  Network.stream_unix_server
    ~socketfile
    ~protocol
    ~no_fork:()
    ()

end (* module UDSCom *)


module Transmission = struct

let transmission ~resistance:r ~channel:ch =
  let order v = Cortex_lib.Channel.send ch v in
  let rec loop prev =
    let next = Cortex.get ~guard:(fun v -> v <> prev) r in
    let () = ignore (order next) in
    loop next
  in
  let init = Cortex.get r in
  let () = ignore (order init) in
  Thread.create loop init

end (* module Transmission *)


let guessServer, _ = 
  let number = 42 in
  let finish ch =
    ch#output_line "You won, see you next time !";
    scream "<Me> Game over.\n";
    ch#output_char ''
  in
  let gaming =
    let rec loop ch = 
      let answer, successUnpack = UDSCom.unpackInt (ch#input_line ()) in
      if successUnpack then 
        let () = scream "<someOne> %d\n" answer in
        match answer with
        | ans when ans > number ->
           ch#output_line "it's less !";
           loop ch
        | ans when ans < number ->
          ch#output_line "it's more !";
          loop ch
        | _ -> 
          ch#output_line "Congratulations !";
          finish ch
      else 
        ch#output_line "Not a number !";
        loop ch
    in
    loop
  in   
  let start ch =
    scream "<Me> There is someone !\n";
    ch#output_line "Hello ! Here is number guessing. Say something so we can start.";
    let msg = ch#input_line () in (* waiting *)
    scream "<SomeOne> %s\n" msg;
    ch#output_line "Guess my number !";
    gaming ch
  in
  UDSCom.server
    ~socketfile:"/tmp/guessServer"
    ~protocol:start

(* let speedWitness = Cortex.return 0

let speedListener = UDSCom.listener
    ~socketfile:"/tmp/speedWitness"
    ~receive:UDSCom.receiveLine
    ~unpack:UDSCom.unpackInt
    ~witness:speedWitness

let speedOrderer = Cortex_lib.Channel.return ()

let speedSender = UDSCom.sender
  ~socketfile:"/tmp/speedOrderer"
  ~send:UDSCom.sendLine
  ~pack:UDSCom.packInt
  ~orderer:speedOrderer

let witnessToOrder = Transmission.transmission
  ~resistance:speedWitness
  ~channel:speedOrderer
*)

let rec loop t =
  if t = 0 then scream "t : 0\n"
  else begin
    (*scream "t : %d\n" t;*) 
    Thread.delay 1.0;
    loop (t - 1)
    end
;;
loop 30 ;;