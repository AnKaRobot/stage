open Cortex
open Cortex_lib

(* Thread efficient printf *)
let scream fmt = Printf.kfprintf flush stdout fmt
