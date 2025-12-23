#PROJET OUVERTURE : algorithmes de génération d'arbres aléatoires
#Fait par Feriel Benatsi, Maya Mebarkia et Dalila Zeghmiche

##DESCRIPTION DU PROJET

Ce projet implémente et compare deux algorithmes de génération aléatoire d'arbres binaires de taille n :
1.  Algorithme de Rémy (version optimisée en O(n)).
2.  Génération ABR (insertion aux feuilles en O(n)).

L'objectif est d'analyser les différences structurelles (hauteur, largeur, équilibre) entre la distribution uniforme (Rémy) et la distribution issue des arbres binaires de recherche (ABR).

##STRUCTURE DU PROJET

*   lib/ : Contient le cœur des algorithmes.
    *   arbre_binaire.ml : Structure de données représentant un arbre binaire.
    *   remy_prototype.ml : Version naive fonctionnelle.
    *   remy_optimal.ml : Version linéaire.
    *   abr.ml : Génération ABR linéaire.
*   test/ : Contient le programme d'expérimentation.
    *   experiment.ml: Lance les mesures et génère `results.csv`.

*   bin/ : (Non utilisé pour les expérimentations).

## Compilation et Exécution

### Prérequis
*   **OCaml** & **Dune**
*   **Python 3** (avec `matplotlib` et `pandas` pour les graphiques)

### 1. Compiler le projet
À la racine du dossier :
```bash
dune build