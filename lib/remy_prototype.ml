(* ========================================================================= *)
(* Question 1.2 — Algorithme de Remy (prototype)                             *)
(* ========================================================================= *)

open Arbre_binaire

(* ------------------------------------------------------------------------- *)
(* Accès et remplacement par position                                         *)
(* ------------------------------------------------------------------------- *)

(* Récupère le sous-arbre correspondant à une position donnée *)
let rec sous_arbre_a_position arbre position =
  match arbre with
  | Feuille ->
      if position = 0 then Feuille
      else failwith "Position invalide"
  | Noeud (g, d) ->
      let total_gauche = nombre_total_noeuds g in
      if position < total_gauche then
        sous_arbre_a_position g position
      else if position = total_gauche then
        arbre
      else
        sous_arbre_a_position d (position - total_gauche - 1)

(* Remplace le sous-arbre à une position donnée *)
let rec remplacer_a_position arbre position nouveau =
  match arbre with
  | Feuille ->
      if position = 0 then nouveau
      else failwith "Position invalide"
  | Noeud (g, d) ->
      let total_gauche = nombre_total_noeuds g in
      if position < total_gauche then
        Noeud (remplacer_a_position g position nouveau, d)
      else if position = total_gauche then
        nouveau
      else
        Noeud (g, remplacer_a_position d (position - total_gauche - 1) nouveau)

(* ------------------------------------------------------------------------- *)
(* Une étape de l'algorithme de Rémy                                          *)
(* ------------------------------------------------------------------------- *)

let etape_remy arbre =
  let total = nombre_total_noeuds arbre in
  let position = Random.int total in

  (* Sous-arbre choisi uniformément *)
  let sous_arbre = sous_arbre_a_position arbre position in

  (* Création du nouveau nœud interne *)
  let nouveau_noeud =
    if Random.bool () then
      Noeud (sous_arbre, Feuille)
    else
      Noeud (Feuille, sous_arbre)
  in

  remplacer_a_position arbre position nouveau_noeud

(* ------------------------------------------------------------------------- *)
(* Génération d'un arbre de taille n                                          *)
(* ------------------------------------------------------------------------- *)

let generer_arbre n =
  Random.self_init ();
  let rec construire k arbre =
    if k = n then arbre
    else construire (k + 1) (etape_remy arbre)
  in
  construire 0 Feuille
