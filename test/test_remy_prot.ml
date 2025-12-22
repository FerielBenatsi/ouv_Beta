open Ouv.Arbre_binaire
open Ouv.Remy_prototype


(* ----------------------------- *)
(* Fonctions de test auxiliaires *)
(* ----------------------------- *)

let rec nb_feuilles = function
  | Feuille -> 1
  | Noeud (g, d) -> nb_feuilles g + nb_feuilles d

let rec est_binaire_plein = function
  | Feuille -> true
  | Noeud (g, d) ->
      est_binaire_plein g && est_binaire_plein d

(* ----------------------------- *)
(* Tests unitaires automatiques  *)
(* ----------------------------- *)

let test_taille n =
  let a = generer_arbre n in
  let ok = (nombre_noeuds_internes a = n) in
  Printf.printf "Test taille n=%d : %s\n"
    n (if ok then "OK" else "ECHEC");
  ok

let test_feuilles n =
  let a = generer_arbre n in
  let ok = (nb_feuilles a = n + 1) in
  Printf.printf "Test feuilles n=%d : %s\n"
    n (if ok then "OK" else "ECHEC");
  ok

let test_binaire_plein n =
  let a = generer_arbre n in
  let ok = est_binaire_plein a in
  Printf.printf "Test binaire plein n=%d : %s\n"
    n (if ok then "OK" else "ECHEC");
  ok

(* ----------------------------- *)
(* Lancement de tous les tests   *)
(* ----------------------------- *)

let run_tests () =
  Random.self_init ();

let tests =
  [ test_taille 0;
    test_taille 1;
    test_taille 5;
    test_taille 10;
    test_taille 50;
    test_taille 100;

    test_feuilles 1;
    test_feuilles 5;
    test_feuilles 10;
    test_feuilles 50;
    test_feuilles 100;



    test_binaire_plein 5;
    test_binaire_plein 10;
    test_binaire_plein 50; 
    test_binaire_plein 100;  ]

  in

  if List.for_all (fun x -> x) tests then
    print_endline "\nTous les tests sont PASSÉS ✅"
  else
    print_endline "\nCertains tests ont ÉCHOUÉ ❌"

let () = run_tests ()
