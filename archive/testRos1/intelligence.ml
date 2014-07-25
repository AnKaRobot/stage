(* #use "/home/jean/WORKING_antoine/intelligence.ml" ;; *)

(* Tools Functions *)
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
| Some next when next > max -> Some max
| Some next when next < min -> Some min
| _                   -> next

let printf fmt = Printf.kfprintf flush stdout fmt

let netcat ?socketfile () =
  Network.stream_unix_server ?socketfile
    ~protocol:begin
	let rec listen channel =
	    let msg = channel#input_line () in (* Attente passive *)
	    Printf.kfprintf flush stdout "Received order: %s\n" msg;
	    listen channel
	in
	listen
       end
    ()

(* Resistances *)
(* let time = Cortex_lib.Clock.make ~limit:60 () *)

let speedOrder = Cortex_lib.Channel.return
    ~on_proposal:(optionAccomodatingRange (- 255) 255)
    ()

let speedWitness = Cortex.return
    ~on_proposal:(notAccomodatingRange (- 1000) 1000)
(*     ~on_commit:(fun old next -> ignore(Cortex_lib.Channel.send speedOrder next)) *)
    0

(* Pour simuler son interlocuteur :
netcat -U /tmp/.intelligenceSockets/speedListener
*)
let protocolSpeedServer =
    let rec listen channel =
        let msg = channel#input_line () in (* Attente passive *)
        let testify speed =
            let () = Cortex.set speedWitness speed in
            printf "From ROS : %s\n" msg
        in
        let () =
            try testify (int_of_string msg)
            with Failure "int_of_string" ->
                printf "Incorrect msg : %s\n" msg
        in
        listen channel
    in
    listen

let protocolSpeedClient =
    let rec write channel =

        (*
        let order = match Cortex.get speedOrder with Some x -> x | _ -> 0 in
        *)
        (* Cette ligne provoque un arrêt du programme *)

        let order = Cortex_lib.Channel.receive speedOrder in
        (* Cette ligne ne termine jamais *)

        let command msg =
            let () = channel#output_line msg in
            printf "To ROS : %s" msg
        in
        let () =
            try command (string_of_int order)
            with Failure "string_of_int" -> print_endline "Incorrect order"
        in
        write channel
    in
    write


(* Threads *)
let speedListener, _ = Network.stream_unix_server
    ~socketfile:"/tmp/.intelligenceSockets/speedListener"
    ~protocol:protocolSpeedServer
    ()

(* Autre solution avec Unix : netcat -lU /tmp/.intelligenceSockets/speedCommander *)
(* let speedCommander_pseudo_ROS_server, _ =
  netcat ~socketfile:"/tmp/.intelligenceSockets/speedCommander" ()*)

(* Pour simuler son interlocuteur :
 netcat -lU /tmp/.intelligenceSockets/speedCommander
*)
let speedCommander = Thread.create(* the protocol with "receive" is blocking *)
    (Network.stream_unix_client
        ~socketfile:"/tmp/.intelligenceSockets/speedCommander"
        ~protocol:protocolSpeedClient)
    ()

(* Cortex_lib.Channel.send speedOrder 42;; *)

(* Start *)
(*let _launchSpeedListener = speedListener () ;;
let _launchSpeedCommander = speedCommander () ;;
Cortex_lib.Channel.send speedOrder 42;;
Cortex_lib.Channel.send speedOrder 43;;
let _launchMaximumTime = Cortex.get ~guard:(fun v -> v >= 5) time*)
