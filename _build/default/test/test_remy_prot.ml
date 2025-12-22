open Ouv.Arbre_binaire
open Ouv.Remy_prototype

(* ----------------------------- *)
(* Test global pour une taille n *)
(* ----------------------------- *)

let test_global n =
  let a = generer_arbre n in

  let ok_noeuds = (nombre_noeuds_internes a = n) in
  let ok_feuilles = (nombre_feuilles a = n + 1) in

  Printf.printf
    "Test n=%d | noeuds=%b | feuilles=%b\n"
    n ok_noeuds ok_feuilles;

  ok_noeuds && ok_feuilles

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
