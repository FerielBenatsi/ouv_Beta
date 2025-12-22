(* ========================================================================= *)
(* Question 1.1 — Structures pour manipuler les arbres binaires              *)
(* ========================================================================= *)

(* Arbre binaire plein :
   - une feuille
   - ou un noeud interne avec exactement deux enfants *)
type arbre_binaire =
  | Feuille
  | Noeud of arbre_binaire * arbre_binaire

(* ------------------------------------------------------------------------- *)
(* Fonctions utilitaires                                                      *)
(* ------------------------------------------------------------------------- *)

(* Nombre de nœuds internes *)
let rec nombre_noeuds_internes = function
  | Feuille -> 0
  | Noeud (g, d) ->
      1 + nombre_noeuds_internes g + nombre_noeuds_internes d

(* Nombre de feuilles *)
let rec nombre_feuilles = function
  | Feuille -> 1
  | Noeud (g, d) ->
      nombre_feuilles g + nombre_feuilles d

(* Nombre total de noeuds (internes + feuilles) *)
let nombre_total_noeuds a =
  nombre_noeuds_internes a + nombre_feuilles a

(* Hauteur de l'arbre *)
let rec hauteur = function
  | Feuille -> 0
  | Noeud (g, d) ->
      1 + max (hauteur g) (hauteur d)
