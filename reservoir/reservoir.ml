open Cortex
open Cortex_lib
open Trace

let time = Clock.make ~delay:5. ()
let factors = return (0.5, 0.2, 0.3) (* p, i, d *)
let mesure = returnT "mesure" currying1 "%f" ~large:() 0.
let reference = returnT "derivee" currying1 "%f" ~large:() 0.
let deriveeTemporal = returnT "deriveeTemporal" currying1 "%f" ~large:() 0.
let sumTemporal = returnT "sumTemporal" currying1 "%f" ~large:() 0.

let prop = viewT
  (fun (m, r) -> r -. m)
  "prop" currying2 "%f %f" currying1 "%f" ~large:()
  (group_pair mesure reference)

let deriveeTemporalCalcul = group_quintupleT
  ~on_proposal:begin fun (t0, tp0, m0, mp0, d0) (t1, tp1, m1, mp1, d1) ->
    let dt = t1 - tp1 in
    if dt = 0
    then (t1, tp1, m1, mp1, d1)
    else 
      let d2 = (m1 -. mp1) /. (float_of_int dt) in
      (t1, t1, m1, m1, d2)
    end
  "deriveeTemporalCalcul" currying5 "(time[%d] timeP[%d] mesu[%f] mesuP[%f]) -> deriv[%f]" ~large:()
  time (return 0) mesure (return 0.) deriveeTemporal

let sumTemporalCalcul = group_quadrupleT
  ~on_proposal:begin fun (t0, tp0, m0, s0) (t1, tp1, m1, s1) ->
  let dt = t1 - tp1 in 
  if dt = 0
  then (t1, tp1, m1, s1)
  else 
    let s2 = s1 +. (m1 *. (float_of_int dt)) in
    (t1, t1, m1, s2)
  end
  "sumTemporalCalcul" currying4 "(time[%d] timeP[%d] mesure[%f]) -> sum[%f]" ~large:()
  time (return 0) mesure sumTemporal

let curryingPID f (p, i, d, (pF, iF, dF)) = f p i d pF iF dF 
let pid = viewT
  (fun (p, i, d, (pF, iF, dF)) -> (p *. pF) +. (i *. iF)  +. (d *. dF) )
  "pid" curryingPID "%f %f %f %f %f %f" currying1 "control[%f]" ~large:()
  (group_quadruple prop sumTemporal deriveeTemporal factors)

let applyControl = group_pairT
  ~on_proposal:begin fun (m0, c0) (m1, c1) -> (c1, c1) end
  "applyControl" currying2 "m[%f] c[%f]" ~large:()
  mesure pid

;;
set reference 100. ;;
set mesure 10. ;;
input_line stdin
