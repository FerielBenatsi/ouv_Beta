open Arbre_binaire

(* Génération d’un arbre binaire de type ABR de taille n *)
let generer_abr n =
  Random.self_init ();

  (* Tableau mutable pour stocker les nœuds *)
  let taille_max = 2 * n + 1 in
  let gauche = Array.make taille_max (-1) in
  let droite = Array.make taille_max (-1) in

  (* Liste des feuilles actives (indices) *)
  let feuilles = ref [0] in

  (* Prochain indice libre *)
  let prochain = ref 1 in

  (* Construction itérative *)
  for _ = 1 to n do
    (* Choix uniforme d’une feuille *)
    let k = Random.int (List.length !feuilles) in
    let feuille = List.nth !feuilles k in

    (* Création des deux nouvelles feuilles *)
    let f_g = !prochain in
    let f_d = !prochain + 1 in
    prochain := !prochain + 2;

    (* Remplacement de la feuille par un nœud interne *)
    gauche.(feuille) <- f_g;
    droite.(feuille) <- f_d;

    (* Mise a jour de la liste des feuilles *)
    feuilles :=
      f_g :: f_d :: List.filter (fun x -> x <> feuille) !feuilles
  done;

  (* Conversion finale en arbre fonctionnel *)
  let rec construire i =
    if gauche.(i) = -1 && droite.(i) = -1 then
      Feuille
    else
      Noeud (construire gauche.(i), construire droite.(i))
  in

  construire 0
