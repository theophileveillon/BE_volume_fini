================================================================================
  prog.exe — Solveur d'advection-diffusion 2D par Volumes Finis (Fortran 90)
================================================================================

  Auteurs  : Théophile VEILLON & MOMO BEB
  Créé le  : 21/05/2026
  Méthode  : Volumes Finis · Euler explicite · Amont (advection) · Centré (diffusion)

  Résolution numérique de l'équation d'advection-diffusion 2D dans un domaine
  rectangulaire, avec champ de vitesse prescrit et conditions de Neumann homogènes
  sur toutes les frontières.

--------------------------------------------------------------------------------
  FICHIERS E/S
--------------------------------------------------------------------------------

  Entrée  : donnees.dat          (paramètres physiques et numériques)
  Sorties : image.png
            sol_000000.vts, sol_000001.vts, ...   (champs 2D format ParaView)

--------------------------------------------------------------------------------
  COMPILATION & EXÉCUTION
--------------------------------------------------------------------------------

  Dans un terminal, se placer dans le dossier contenant les sources (*.f90,
  Makefile), puis taper :

    make run MODE=<mode_choisi>

--------------------------------------------------------------------------------
  MODES DISPONIBLES
--------------------------------------------------------------------------------

  [principal]
    classique
        Géométrie complète avec advection et diffusion actives.

  [tests unitaires]
    advection_pure_horizontale
        Advection 1D en x. Concentration C1 à gauche, C0 à droite.
    advection_pure_verticale
        Advection 1D en y. Concentration C1 en bas, C0 en haut.
    diffusion_pure_horizontale
        Diffusion 1D en x. Concentration C1 à gauche, C0 à droite.
    diffusion_pure_verticale
        Diffusion 1D en y. Concentration C1 en bas, C0 en haut.
    advection_pure_verticale_CFL
        Balayage paramétrique CFL ∈ [1, 2] en advection verticale.
    diffusion_pure_horizontale_R
        Balayage paramétrique R ∈ [0.01, 1.1] en diffusion horizontale.
    diffusion_pure_verticale_R
        Balayage paramétrique R ∈ [0.01, 1.1] en diffusion verticale.

  [analyses]
    peclet_stationnaire
        Prend un nombre de Péclet Pe en entrée terminal.
        Retourne le temps t_stat auquel la solution est stationnaire
        (critère ordre 2, précision ε = 1e-3).
    convergence_maillage
        Compare plusieurs raffinements de maillage pour évaluer
        la convergence en maillage.
    peclet_diffusion
        Trace l'évolution de la concentration et du temps t95% en
        fonction de Pe ∈ [1e-2, 1e6] — variation de la diffusivité κ.
    peclet_advection
        Même analyse que peclet_diffusion, mais avec variation
        de la vitesse α à la place de κ.
    peclet_finale
        Trace l'evolution des grandeurs adimenssionnées t95%/tau_diffusion
         et t95%/tau_advection en fonction du peclet

--------------------------------------------------------------------------------
  VISUALISATION PARAVIEW
--------------------------------------------------------------------------------

  En mode classique :

    cd rendu_paraview/   (nom du dossier configurable dans le Makefile)
    paraview &

================================================================================