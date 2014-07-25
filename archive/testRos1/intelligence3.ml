(* Thread-efficient printf *)
let scream fmt = Printf.kfprintf flush stdout fmt

(* Just for debugging: *)
let () = begin
  Printexc.record_backtrace true;
  Ocamlbricks_log.enable ();
  end

module Res = struct
  let x = Cortex.return 0
end
let y = Res.x

module UDSCom = struct

let formatInt msg =
  try (int_of_string msg), true
  with _ -> 10, false

(* low level listener structure *)
let listener socketfile =
  let protocol =
    let rec listen channel =
      let msg = channel#input_line () in
      let speed, result = formatInt msg in
      match result with
      | true ->
          let previous = Cortex.get Res.x in
          let accepted, has_changed = Cortex.propose Res.x speed in
          scream "Has changed : %b, Accepted speed : %d (previous %d) (x==y ? %b)\n" has_changed accepted previous (y==Res.x);
          listen channel
      | false ->
          scream "Strange message : %s\n" msg
    in
    listen
  in
  Network.stream_unix_server
    ~socketfile
    ~protocol
    ~no_fork:()
    ()

end (* module UDSCom *)

let l1 = UDSCom.listener "/tmp/r1" ;;

let rec loop i =
  let v = Cortex.get Res.x in
  if v = 42 || i > 20 then () else begin
    scream "still not : %d (x==y ? %b)\n" v (y==Res.x);
    Thread.delay 1.0;
    loop (i+1)
    end
;;

loop 0 ;;
