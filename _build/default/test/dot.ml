
open Ouv.Remy_optimal
open Ouv.Graphviz

let () =
  Random.self_init ();
(*le 0 genere une feuille *)
  let a = generer_arbre 5 in
  ecrire_dot_stylise "arbre.dot" a;

  print_endline "Fichier arbre.dot généré (Graphviz)"
