(* Tristan Bersoux et Nicolas Compère*)
(*OCaml version 4.11.1*)

type place = Place of int
type transition = Transition of int
type marquage = (place*int) list
type arc = ArcEntrant of place*transition*int | ArcSortant of transition*place*int
type reseau = arc list*arc list*marquage

(*=========Getters des enregistrements=========*)
let num_of_place p= 
  match p with
  |Place (r) -> r ;;

let num_of_transition t =
  match t with
  |Transition (r) -> r;;

let place_of_marquage m =
  match m with
  |(p,j) -> p ;;

let jeton_of_marquage m =
  match m with
  |(p,j) -> j ;;

let place_of_arc a =
  match a with
  |ArcEntrant (pl,t,po) -> pl
  |ArcSortant (t,pl,po) -> pl ;;

let transition_of_arc a =
  match a with
  |ArcEntrant (pl,t,po) -> t
  |ArcSortant (t,pl,po) -> t ;;

let poids_of_arc a =
  match a with
  |ArcEntrant (pl,t,po) -> po
  |ArcSortant (t,pl,po) -> po ;;

let entrants_of_reseau r =
  match r with
  |(e,s,m) -> e ;;

let sortants_of_reseau r =
  match r with
  |(e,s,m) -> s ;;

let marquage_of_reseau r =
  match r with
  |(e,s,m) -> m ;;
(*=========Fin Getters des enregistrements=========*)

(*Appliquer un marquage m a un réseau r (==renvoie un nouveau r qui a les arcs de r mais avec le marquage m)*)
let marquer_reseau m r=
  match r with
  |(e,s,_) -> (e,s,m)

(*Recupère le nombre de jetons de la place i dans le marquage m *)
let rec jetons_of_place i m=
  match m with
  |[] -> 0
  |(pl,j)::q -> if(num_of_place pl)=i then j else jetons_of_place i q ;;

(*Modifie le nombre de jetons j dans la place i et renvoie un nouveau marquage*)
let rec set_jetons_of_place i j m =
  match m with
  |[] -> []
  |(pl,jet)::q -> if(num_of_place pl)=i then [pl,j]@q else [pl,jet]@(set_jetons_of_place i j q) ;;

(* Renvoie la liste des arcs entrants de la transition numérotée nt *)
let arcsE_of_transition nt r =
  let arcsE = entrants_of_reseau r in
  let rec aux arcs nt =
    match arcs with 
    |ArcEntrant (pl,t,po)::q -> if (num_of_transition t)=nt then [ArcEntrant (pl,t,po)]@(aux q nt) else aux q nt
    |_ -> []
  in aux arcsE nt;;

(* Renvoie la liste des arcs sortants de la transition numérotée nt *)
let arcsS_of_transition nt r =
  let arcsS = sortants_of_reseau r in
  let rec aux arcs nt =
    match arcs with 
    |ArcSortant (t,pl,po)::q -> if (num_of_transition t)=nt then [ArcSortant (t,pl,po)]@(aux q nt) else aux q nt
    |_ -> []
  in aux arcsS nt;;

(*Vérifie si tous les poids entrants des arcs d'une transition sont en accord avec les jetons des places correspondantes *)
let rec poids_ok nt r=
  let arcsE = arcsE_of_transition nt r in
  let rec aux arcs =
    match arcs with
    |ArcEntrant (pl,t,po)::q -> ((jetons_of_place (num_of_place pl) (marquage_of_reseau r))  >= po)&&(aux q)
    |_ -> true
  in aux arcsE
;;

(*Vérifie si une transition numérotée nt est franchissable *)  
let estFranchissable nt r= 
  poids_ok nt r
  (*  si on utilise le fait que jeton max dans place on doit ajouter une autre fonction*)
  (* fonction qui vérifie les jetons max des places de la transition nt si on a le temps elle sera faite, sinon extension possible *)
  (* Il faut ajouter un entier au marquage pour chaque place, et vérifier pour chaque arc sortant de la transition nt 
  si jeton de la place de la transition + poids de la transition ne dépasse pas la capacité maximum, et apeller cete fonction auxiliaire dans estFranchissable*)
;; 

(* Active un arc, soit en consommation si entrant, soit en production si sortant *)
(* Prend m le marquage initial et renvoie le nouveau marquage *)
let activer_arc arc m =
  let rec modifierMarquage pl new_j m =
    match m with
    |[] -> []
    |(p,j)::q -> if p=pl then [p,new_j]@(modifierMarquage pl new_j q) else [p,j]@(modifierMarquage pl new_j q)
  in
  match arc with
  |ArcEntrant (pl,tr,po) -> modifierMarquage pl ((jetons_of_place(num_of_place pl)m)-po) m
  |ArcSortant (tr,pl,po) -> modifierMarquage pl ((jetons_of_place(num_of_place pl)m)+po) m
;;

(* Active une transition donnée par son numéro et renvoie le marquage résultant*)
let activer_transition nt r=
  if estFranchissable nt r then
    begin
      let arcs_entrants = arcsE_of_transition nt r in
      let arcs_sortants = arcsS_of_transition nt r in
      let marquage_init = marquage_of_reseau r in
      let rec auxEntrants arcs m =
        match arcs with
        |[]-> m
        |t::q -> auxEntrants q (activer_arc t m)
      in
      let rec auxSortants arcs m =
        match arcs with
        |[] -> m
        |t::q -> auxSortants q (activer_arc t m)
      in
      auxSortants arcs_sortants (auxEntrants arcs_entrants marquage_init)
    end
  else marquage_of_reseau r
;;

(*Renvoie le réseau r après activation de la transition nt*)
let step nt r =
  marquer_reseau (activer_transition nt r) r;;

(*L'exemple est donné graphiquement dans le rapport*)

(* exemple écrit de manière exhaustive*)

(*Places*)
let p1 = Place 1
let p2 = Place 2
let p3 = Place 3
(*Transition*)
let t1 = Transition 1
let t2 = Transition 2
(*Arcs entrants*)
let arcE1 = ArcEntrant (p1,t1,1)
let arcE2 = ArcEntrant (p2,t2,1)
(*Arcs sortants*)
let arcS1 = ArcSortant (t1,p2,1)
let arcS2 = ArcSortant (t2,p3,2)
let arcS3 = ArcSortant (t2,p1,1)
(*Marquage initial*)
let marque = [(p1,1);(p2,0);(p3,0)]
(*Déclaration du réseau*)
let res = (
  [arcE1;arcE2],
  [arcS1;arcS2;arcS3],
  marque
)

(* même exemple écrit de manière plus compacte *)

let res = (
  [
    ArcEntrant(Place 1,Transition 1,1);
    ArcEntrant(Place 2,Transition 2,1)
  ],
  [
    ArcSortant(Transition 1,Place 2,1);
    ArcSortant(Transition 2,Place 3,2);
    ArcSortant(Transition 2,Place 1,1)
  ],
  [
    (Place 1,1);
    (Place 2,0);
    (Place 3,0)
  ]
)



(*Exemple de fonction qui simule un fonctionnement du réseau*)
(*Pas super utile mais double un entier i donné en paramètres*)
let rec doubler i r =
  match i with
  |0 -> jetons_of_place 3 (marquage_of_reseau r)
  |_ -> doubler (i-1) (step 2 (step 1 r))
;;

let resPostInutile = step 2 res;;
let resPostT1 = step 1 resPostInutile;;
let resPostT2 = step 2 resPostT1;;