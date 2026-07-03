# Workflow de verification et fusion - trialdesign

Procedure a suivre sur la branche de developpement, avant de fusionner
vers la branche principale (`main`).

## 1. Verifier le package (dans R / RStudio)

Depuis la racine du package :

```r
source("dev/check_package.R")
```

Ce script :
- regenere la documentation (`devtools::document()`)
- fait un audit rapide des dependances
- lance `devtools::check()` complet
- affiche un resume : pret a fusionner, ou pas encore

**Ne pas continuer tant que le resume n'affiche pas 0 erreur / 0 warning.**
Les notes restantes (ex: horloge systeme) sont generalement tolerables.

## 2. Relancer le script de test global (si du code a change)

```r
devtools::load_all()
source("test_ssdesignr.R")
```

Verifier dans le recapitulatif final qu'il n'y a aucun `ECHEC`.

## 3. Verifier l'etat Git (dans un terminal, pas dans R)

```bash
git status
git branch
```

Confirmer que vous etes bien sur la branche de developpement, et lister
les fichiers modifies.

## 4. Ajouter et committer les changements

```bash
git add .
git commit -m "Description courte des changements effectues"
```

Exemples de bons messages de commit :
- `"Correction formule DEFF stepped-wedge + validation numerique"`
- `"Ajout sample_size_phase2 et sample_size_precision + tests"`
- `"Nettoyage check() : dependances, accents, globals.R"`

## 5. Fusionner vers la branche principale

```bash
git checkout main
git merge ma-branche-dev
```

(Remplacer `ma-branche-dev` par le nom reel de votre branche.)

## 6. Verification finale sur main

Une fois la fusion faite, relancer le check une derniere fois pour
confirmer que tout fonctionne toujours correctement sur `main` :

```r
source("dev/check_package.R")
```

## Astuce : ajouter un nouveau fichier/dossier a ignorer par R CMD check

Si un nouveau dossier de developpement doit etre exclu des verifications
R CMD check (comme `dev/` ou `test_outputs_*/` l'ont ete) :

```r
usethis::use_build_ignore("nom_du_dossier_ou_fichier")
```

Cette commande modifie directement `.Rbuildignore` -- elle n'a pas besoin
d'etre relancee ensuite, sauf pour un nouvel element a exclure.
