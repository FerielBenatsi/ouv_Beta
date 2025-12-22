open Ouv.Arbre_binaire
open Ouv.Remy_prototype

(* ----------------------------- *)
(* Fonctions auxiliaires         *)
(* ----------------------------- *)

let rec nb_feuilles = function
  | Feuille -> 1
  | Noeud (g, d) -> nb_feuilles g + nb_feuilles d

let rec est_binaire_plein = function
  | Feuille -> true
  | Noeud (g, d) -> est_binaire_plein g && est_binaire_plein d

(* ----------------------------- *)
(* Test global pour une taille n *)
(* ----------------------------- *)

let test_global n =
  let a = generer_arbre n in
  let plein = est_binaire_plein a in
  let noeuds = (nombre_noeuds_internes a = n) in
  let feuilles = (nb_feuilles a = n + 1) in

  Printf.printf
    "Test n=%d | plein=%b | noeuds=%b | feuilles=%b\n"
    n plein noeuds feuilles;

  plein && noeuds && feuilles

(* ----------------------------- *)
(* Lancement des tests           *)
(* ----------------------------- *)

let run_tests () =
  Random.self_init ();

  let tests =
    [ test_global 1000;
      test_global 100;
      test_global 50;
      test_global 10;
      test_global 5;
      test_global 1;
      test_global 0 ]
  in

  if List.for_all (fun x -> x) tests then
    print_endline "\nTous les tests sont PASSÉS ✅"
  else
    print_endline "\nCertains tests ont ÉCHOUÉ ❌"

let () = run_tests ()
