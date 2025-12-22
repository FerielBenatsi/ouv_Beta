open Arbre_binaire

type cellule = {
  mutable gauche : int;
  mutable droite : int;
  mutable parent : int;
  mutable est_gauche : bool;
}

let creer_vide () = { 
  gauche = -1; 
  droite = -1; 
  parent = -1; 
  est_gauche = false 
}

let tableau_vers_arbre tab racine_idx =
  let rec construire i =
    if i = -1 then Feuille
    else
      let noeud = tab.(i) in
      if noeud.gauche = -1 && noeud.droite = -1 then Feuille
      else Noeud (construire noeud.gauche, construire noeud.droite)
  in
  construire racine_idx

let generer_arbre n =
  if n = 0 then Feuille
  else begin
    let taille_max = 2 * n + 2 in
    let tab = Array.init taille_max (fun _ -> creer_vide ()) in

    let racine = ref 0 in
    let nb_noeuds = ref 1 in 
    
    for _ = 1 to n do
      let cible = Random.int !nb_noeuds in
      
      let nouv_interne = !nb_noeuds in
      let nouv_feuille = !nb_noeuds + 1 in
      nb_noeuds := !nb_noeuds + 2;
      
      let parent_cible = tab.(cible).parent in
      let cible_etait_gauche = tab.(cible).est_gauche in

      tab.(nouv_interne).parent <- parent_cible;
      tab.(nouv_interne).est_gauche <- cible_etait_gauche;
      
      if parent_cible <> -1 then (
        if cible_etait_gauche then tab.(parent_cible).gauche <- nouv_interne
        else tab.(parent_cible).droite <- nouv_interne
      ) else (
        racine := nouv_interne
      );

      if Random.bool () then (
        tab.(nouv_interne).gauche <- cible;
        tab.(nouv_interne).droite <- nouv_feuille;
        
        tab.(cible).est_gauche <- true;
        tab.(nouv_feuille).est_gauche <- false;
      ) else (
        tab.(nouv_interne).gauche <- nouv_feuille;
        tab.(nouv_interne).droite <- cible;
        
        tab.(nouv_feuille).est_gauche <- true;
        tab.(cible).est_gauche <- false;
      );
      
      tab.(cible).parent <- nouv_interne;
      tab.(nouv_feuille).parent <- nouv_interne;
    done;
    
    tableau_vers_arbre tab !racine
  end