open Cortex
open Cortex_lib

(* Thread efficient printf *)
let scream fmt = Printf.kfprintf flush stdout fmt





(* *** Le problème de la somme *** *)
(* Soit une résistance (ou un groupe) [(..., s, ...)] *)
(* 1) à chaque fois que s est l'objet d'un set avec une valeur x, *)
(*    ajouter x à s *)
(* 2) lorsque on get la valeur, c'est la somme s qui sort *)
(* 3) on peut éventuellement prévoir de remettre s à zéro, mais ce *)
(*    sera de toute façon possible avec un set (-s) *)


(* Réponse 1 : une solution simple avec un booléen *)
(* Utiliser un booléen "sommer" en plus de la valeur s *)
(* si sommer est true, ajoute à la somme précédente  *)
(* si sommer est false, remplace la somme précedente  *)
let sommer =
  let on_proposal (b0, s0) (b1, s1) =
    match b1 with
    | true -> (false, s0 +. s1)
    | false -> (false, s1)
  in
  return ~on_proposal (false, 0.)

(* Ce truc du booléen est largement utilisable dans d'autres *)
(* situations pour forcer l'accord de on_proposal *)

(* Réponse 2 : sommer à intervalle de temps fixe *)
(* On maintient valeur de somme s, une mesure x, on ajoute une horloge *)
(* et calculer la somme à chaque changement de l'horloge. La difficulté est *)
(* de savoir si on vient de  changer d'horloge ou si la somme vient *)
(* d'être calculée et que c'est la seconde fois que on_proposal est *)
(* appelée. Pour distinguer les deux cas, il nous faut un hack et une *)
(* troisième valeur (booléen ou  autre). *)

(* Plutot qu'un booléen on utilise un type énuméré mais c'est le même *)
(* esprit que pour la réponse 1. *)
type todo = Todo | Done

let sommer =
  (* l'horloge suivante se déclenche toutes les trois secondes *)
  let horloge = Clock.make ~delay: 3. () in
  (* on lui adjoint une donnée (un flottant à sommer), *)
  let data = return 0. in
  (* la somme *)
  let somme = return 0. in
  (* et un tag de sommation (qui va poser problème) *)
  let gtd = return Done in
  (* Le tout dans un groupe avec ces quatre resistances *)
  let on_proposal (h0, d0, s0, gtd0) (h1, d1, s1, gtd1) =
    match (h0 < h1, gtd1) with
    | (false, _) -> (h0, d1, s0, gtd0) (* h0 = h1 => ne rien faire *)
    | (true, Todo) -> let dt = float_of_int (h1 - h0) in
                      (h1, d1, s0 +. dt *. d1, Done)
    | (true, Done) -> (h1, d1, s1, Done) (* done => accepter *)
  in
  group_quadruple
    ~on_proposal
    horloge data somme gtd

(* Le problème est que cette sommation n'aura pas lieu à chaque *)
(* changement d'horloge comme on devrait l'espérer pour une horloge *)
(* assez lente par rapport à la commutation des threads. Le coupable *)
(* est bien entendu gtd qui reste éternellement à Done et qu'il *)
(* faudrait réarmer manuellement à chaque changement de valeur. C'est *)
(* possible mais on n'aura pas une somme bien à jour si les *)
(* changements de valeurs sont peu fréquents. *)

(* Réponse 3 : améliorons la réponse 2 *)
(* on détermine si on est Done ou Todo à l'aide de l'horloge en *)
(* utilisant comme quatrième valeur la dernière valeur d'horloge *)
(* traitée par sommation. autrement dit il s'agit de sommer une suite *)
(* indexée par le temps et on note dans une quatrième variable, en *)
(* plus du temps réel, de la donnée et de la somme, l'indice du *)
(* dernier élément sommé. *)
(* On profite également de cette amélioration pour passer la mesure à *)
(* sommer en paramètre (une resistance contenant un flottant qui peut *)
(* par exemple provenir d'un dispositif physique *)

(* data est la mesure à sommer de type float Cortex.t. *)
let sommer
    ?(amortissement= 0.)
    ?(delay=1.0)
    ?(valeur_initiale=0.)
    ?(horloge)
        data  =
  let horloge = match horloge with
    (* utiliser l'horloge fournie *)
    | Some h -> h
    (* sinon une nouvelle horloge, qui se déclenchera toutes les delay *)
    (* secondes (par défaut une seconde) *)
    | None -> Clock.make ~delay ()
  in
  (* la somme *)
  let somme = return valeur_initiale in
  (* et l'indice du dernier élément à avoir été sommé *)
  let dernier_indice = return 0 in
  (* Et on forme un groupe avec ces resistances *)
  let on_proposal (h0, s0, d0, i0) (h1, s1, d1, i1) =
    match (h0 < h1, i1 < h1) with
    | (false, _) -> (h0, s0, d1, h0) (* h0 = h1 => ne rien faire *)
    | (true, true) -> let dt = float_of_int (h1 - h0) in
                      let somme_amortie =
                        (1. -. amortissement) *. s0
                        +. (1. +. amortissement) *. dt *. d1 in
                      (h1, somme_amortie, d1, h1)
    | (true, false) -> (h1, s1, d1, h1) (* i1 = t1 => accepter *)
  in ignore
  (group_quadruple
     ~on_proposal
     horloge somme data dernier_indice);
  somme


let deriver ?(delay=1.0) ?(horloge) data  =
  let horloge = match horloge with
    (* utiliser l'horloge fournie *)
    | Some h -> h
    (* sinon une nouvelle horloge, qui se déclenchera toutes les delay *)
    (* secondes (par défaut une seconde) *)
    | None -> Clock.make ~delay ()
  in
  (* la dérivée *)
  let derivee = return 0. in
  (* et l'indice du dernier élément à avoir été sommé *)
  let dernier_terme = return (get data, 0) in
  (* Et on forme un groupe avec ces resistances *)
  let on_proposal (h0, d0, x0, (u0, i0)) (h1, d1, x1, (u1, i1)) =
    match (h0 < h1, i1 < h1) with
    | (false, _) -> (h0, d0, x1, (u0, i0)) (* h0 = h1 => ne rien faire *)
    | (true, true) -> let dt = float_of_int (h1 - i0) in
                      let derivee = (x1 -. u0) /. dt in
                      (h1, derivee, x1, (x1, h1))
    | (true, false) -> (h1, d1, x1, (u1, h1)) (* i1 = t1 => accepter *)
  in ignore
  (group_quadruple
     ~on_proposal
     horloge derivee data dernier_terme);
  derivee


(* Exemple d'usage *)
(* Problème 1 : ce programme givre. *)
(* Problème 2 : la dérivée calcule faux. *)
let () =
  let pour_pendule = Clock.make ~delay:10.0 () in
  let acceleration = return 0. in
  let () =
    let on_proposal (h0, a0) (h1, a1) =
      match h1 mod 3 with
      | 1 -> (h1, 1.)
      | 2 -> (h1, -1.)
      | _ -> (h1, 0.)
    in
    ignore (group_pair ~on_proposal pour_pendule acceleration)
  in
  let horloge = Clock.make ~delay:1.0 () in
  let vitesse = sommer acceleration in
  let position = sommer vitesse in
  let a = deriver (deriver position) in
  (* let a = return 42. in *)
  let log = group_quintuple
    ~on_proposal: (fun (t0,_,_,_,_) (t,a1,m1,v1,p1)  ->
      if (t > t0) then
        (scream "%05d\t acceleration : %.1f (%.1f mesuree) \t vitesse : %.1f\t position : %.1f\n"
           t
           a1
           m1
           v1
           p1
        );
      (t,a1,m1,v1,p1)
    )
    horloge acceleration a vitesse position in
  ignore log;
  ignore (input_line stdin)
