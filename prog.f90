program main
    use m_type
    use sousprog
    implicit none
    type(noueur) :: noeud
    type(phys) :: p
    type(num) :: n
    type(grid) :: g
    type(statistique) :: Stat
    real, dimension(:,:), allocatable :: C_futur
    real :: time, Tf, c_moy
    integer :: i, i_clf, i_R, i_vol, i_pe, Step
    real, dimension(5) :: CLF_values, R_values
    integer, dimension(5) :: nx_values, ny_values
    real, dimension(:), allocatable :: c_max

    !-----------------------------------------------------------------------------#
    !pour le makefile
    !-----------------------------------------------------------------------------#

    integer, parameter         :: MAX_MODES = 20
    character(len=256)         :: dossier
    character(len=512)         :: mode_list_raw
    character(len=64)          :: mode_list(MAX_MODES)
    character(len=64)          :: mode
    integer                    :: n_modes, i_modes, pos_modes, next_modes
    logical                    :: found_modes

    ! Récupère le dossier (arg 1)
    call get_command_argument(1, dossier)
    dossier = trim(dossier)

    ! recuperation des modes
    call get_command_argument(2, mode_list_raw)   ! la liste entière
    call get_command_argument(3, mode)

    ! Découpage de mode_list_raw sur les espaces
    n_modes = 0
    mode_list_raw = adjustl(mode_list_raw)
    pos_modes = 1
    do while (pos_modes <= len_trim(mode_list_raw))
        next_modes = index(mode_list_raw(pos_modes:), ' ')
        if (next_modes == 0) then
            n_modes = n_modes + 1
            mode_list(n_modes) = trim(mode_list_raw(pos_modes   :))
            exit
        end if
        if (next_modes > 1) then
            n_modes = n_modes + 1
            mode_list(n_modes) = mode_list_raw(pos_modes:pos_modes+next_modes-2)
        end if
        pos_modes = pos_modes + next_modes
    end do

    ! Vérifie que le mode est légitime
    found_modes = .false.
    do i_modes = 1, size(mode_list)
        if (trim(mode_list(i_modes)) == trim(mode)) then
            found_modes = .true.
            exit
        end if
    end do

    if (.not. found_modes) then
        print *, "argument MODE doit être l'un des suivants :"
        do i_modes = 1, size(mode_list)
            print *, " - ", trim(mode_list(i_modes))
        end do
        stop 1
    end if

    !-----------------------------------------------------------------------------#
    !fin de la partie makefile
    !-----------------------------------------------------------------------------#

    !lit les données du problème
    call reader('donnees.dat', p, n)

    !initialisation des tableaux et les paramètres du problème
    allocate(g%c(n%nx, n%ny))
    allocate(C_futur(n%nx, n%ny))

    !initialisation des vitesses u et v
    allocate(g%u(n%nx+1 , n%ny))
    allocate(g%v(n%nx, n%ny+1))

    !Pour vts_writer
    allocate(noeud%x(n%nx +1, n%ny +1))
    allocate(noeud%y(n%nx +1, n%ny +1))
    call init_noeud(p, n, noeud)

    if (mode == 'classique') then

        !initialisation de delta_x, delta_y, delta_t, C et des vitesses u et v
        n%delta_x = 2 * p%l / n%nx
        n%delta_y = p%l / n%ny
        call init_c(g%c, p, n)
        call init_v(g%u, g%v, p, n)
        call calc_delta_t(g, p, n)

        ! boucle temporelle pour avoir C a chaque pas de temps
        time = 0.
        tf = (n%nb_ite-1) * n%dt
        Step = 1

        do i = 1, n%nb_ite
            call writer(n, g, noeud, time, tf, Step, dossier)
            call calc_c_t_dt(C_futur, g, p, n)
            g%c = C_futur
            time = time + n%dt
            Step = Step + 1
        end do
    end if


    if(mode == 'advection_pure_verticale') then

        p%kappa = 0. !advection pure
        n%nb_ite = 50 ! pour une meilleur courbe

        !initialisation de delta_x, delta_y et delta_t et C
        n%delta_x = 2 * p%l / n%nx
        n%delta_y = p%l / n%ny
        call init_c_verticale(g%c, p, n)
        call init_v_uniforme_verticale(g%u, g%v, p, n)
        call calc_delta_t(g, p, n)

        ! boucle temporelle pour avoir C a chaque pas de temps
        time = 0.
        tf = (n%nb_ite-1) * n%dt
        Step = 1
        call writer_python_advection_pure_verticale(n, p, g, 0)
        do i = 1, n%nb_ite
            call writer(n, g, noeud, time, tf, Step, dossier)
            call writer_python_advection_pure_verticale(n, p, g, 1)
            call calc_c_t_dt(C_futur, g, p, n)
            g%c = C_futur
            time = time + n%dt
            Step = Step + 1
        end do
    end if


    if(mode == 'advection_pure_verticale_CLF') then
        
        p%kappa = 0. !advection pure
        n%nb_ite = 50 !pour de meilleur courbes

        !initialisation de delta_x, delta_y et delta_t et C
        n%delta_x = 2 * p%l / n%nx
        n%delta_y = p%l / n%ny
        CLF_values = [1., 1.25, 1.5, 1.75,2.]
        do i_clf=1, 5
            n%CLF = CLF_values(i_clf)
            call init_c_verticale(g%c, p, n)
            call init_v_uniforme_verticale(g%u, g%v, p, n)
            call calc_delta_t(g, p, n)

            ! boucle temporelle pour avoir C a chaque pas de temps
            time = 0.
            tf = (n%nb_ite-1) * n%dt
            Step = 1
            call writer_python_advection_pure_verticale_CLF(n, p, g, 0, i_clf)
            do i = 1, n%nb_ite
                call writer(n, g, noeud, time, tf, Step, dossier)
                call writer_python_advection_pure_verticale_CLF(n, p, g, 1, i_clf)
                call calc_c_t_dt(C_futur, g, p, n)
                g%c = C_futur
                time = time + n%dt
                Step = Step + 1
            end do
        end do
    end if


    if(mode == 'advection_pure_horizontale') then
        
        p%kappa = 0. !advection pure
        n%nb_ite = 25 ! pour de meilleurs courbes

        !initialisation de delta_x, delta_y et delta_t et C
        n%delta_x = 2 * p%l / n%nx
        n%delta_y = p%l / n%ny
        call init_c_horizontale(g%c, p, n)
        call init_v_uniforme_horizontale(g%u, g%v, p, n)
        call calc_delta_t(g, p, n)

        ! boucle temporelle pour avoir C a chaque pas de temps
        time = 0.
        tf = (n%nb_ite-1) * n%dt
        Step = 1
        call writer_python_advection_pure_horizontale(n, p, g, 0)
        do i = 1, n%nb_ite
            call writer(n, g, noeud, time, tf, Step, dossier)
            call writer_python_advection_pure_horizontale(n, p, g, 1)
            call calc_c_t_dt(C_futur, g, p, n)
            g%c = C_futur
            time = time + n%dt
            Step = Step + 1
        end do
    end if


    if (mode == 'diffusion_pure_horizontale') then

        p%alpha = 0. !diffusion pure
        n%nb_ite = 50

        !initialisation de delta_x, delta_y, delta_t, C et des vitesses u et v
        n%delta_x = 2 * p%l / n%nx
        n%delta_y = p%l / n%ny
        call init_c_horizontale(g%c, p, n)
        call init_v_uniforme_horizontale(g%u, g%v, p, n)
        call calc_delta_t(g, p, n)

        ! boucle temporelle pour avoir C a chaque pas de temps
        time = 0.
        tf = (n%nb_ite-1) * n%dt
        Step = 1
        call writer_python_diffusion_pure_horizontale(n, p, g, 0)
        do i = 1, n%nb_ite
            call writer(n, g, noeud, time, tf, Step, dossier)
            call writer_python_diffusion_pure_horizontale(n, p, g, 1)
            call calc_c_t_dt(C_futur, g, p, n)
            g%c = C_futur
            time = time + n%dt
            Step = Step + 1
        end do
    end if


    if (mode == 'diffusion_pure_horizontale_R') then
        
        p%alpha = 0. !diffusion pure
        n%nb_ite = 50
        !initialisation de delta_x et delta_y
        n%delta_x = 2 * p%l / n%nx
        n%delta_y = p%l / n%ny

        R_values = [0.01, 0.5, 0.75, 1., 1.1]

        do i_R = 1, 5
            n%R = R_values(i_R)
            !initialisation de delta_t, C et des vitesses u et v
            call init_c_horizontale(g%c, p, n)
            call init_v_uniforme_horizontale(g%u, g%v, p, n)
            call calc_delta_t(g, p, n)

            ! boucle temporelle pour avoir C a chaque pas de temps
            time = 0.
            tf = (n%nb_ite-1) * n%dt
            Step = 1
            call writer_python_diffusion_pure_horizontale_R(n, p, g, 0, i_R)
            do i = 1, n%nb_ite
                call writer(n, g, noeud, time, tf, Step, dossier)
                call writer_python_diffusion_pure_horizontale_R(n, p, g, 1, i_R)
                call calc_c_t_dt(C_futur, g, p, n)
                g%c = C_futur
                time = time + n%dt
                Step = Step + 1
            end do
        end do
    end if


    if (mode == 'diffusion_pure_verticale') then

        p%alpha = 0. !diffusion pure
        n%nb_ite = 100

        !initialisation de delta_x, delta_y, delta_t, C et des vitesses u et v
        n%delta_x = 2 * p%l / n%nx
        n%delta_y = p%l / n%ny
        call init_c_verticale(g%c, p, n)
        call init_v_uniforme_verticale(g%u, g%v, p, n)
        call calc_delta_t(g, p, n)

        ! boucle temporelle pour avoir C a chaque pas de temps
        time = 0.
        tf = (n%nb_ite-1) * n%dt
        Step = 1
        call writer_python_diffusion_pure_verticale(n, p, g, 0)
        do i = 1, n%nb_ite
            call writer(n, g, noeud, time, tf, Step, dossier)
            call writer_python_diffusion_pure_verticale(n, p, g, 1)
            call calc_c_t_dt(C_futur, g, p, n)
            g%c = C_futur
            time = time + n%dt
            Step = Step + 1
        end do
    end if


    if (mode == 'diffusion_pure_verticale_R') then
        
        p%alpha = 0. !diffusion pure
        n%nb_ite = 50

        !initialisation de delta_x et delta_y
        n%delta_x = 2 * p%l / n%nx
        n%delta_y = p%l / n%ny

        R_values = [0.01, 0.5, 0.75, 1., 1.1]

        do i_R = 1, 5
            n%R = R_values(i_R)
            !initialisation de delta_t, C et des vitesses u et v
            call init_c_verticale(g%c, p, n)
            call init_v_uniforme_verticale(g%u, g%v, p, n)
            call calc_delta_t(g, p, n)

            ! boucle temporelle pour avoir C a chaque pas de temps
            time = 0.
            tf = (n%nb_ite-1) * n%dt
            Step = 1
            call writer_python_diffusion_pure_verticale_R(n, p, g, 0, i_R)
            do i = 1, n%nb_ite
                call writer(n, g, noeud, time, tf, Step, dossier)
                call writer_python_diffusion_pure_verticale_R(n, p, g, 1, i_R)
                call calc_c_t_dt(C_futur, g, p, n)
                g%c = C_futur
                time = time + n%dt
                Step = Step + 1
            end do
        end do
    end if


    if (mode == 'peclet_stationnaire') then

        !choix du Peclet
        print *, "Entrez un nombre de Peclet :"
        read *, n%Pe
        P%kappa = p%alpha*p%l/n%Pe
        n%nb_ite = 3000


        Stat%epsilon = 1.e-3

        !initialisation des esperance, variance et covariance
        allocate(Stat%E(n%nb_ite))
        allocate(Stat%Var(n%nb_ite))
        allocate(Stat%Covar(n%nb_ite))

        !initialisation de delta_x, delta_y, delta_t, C et des vitesses u et v
        n%delta_x = 2 * p%l / n%nx
        n%delta_y = p%l / n%ny
        call init_c(g%c, p, n)
        call init_v(g%u, g%v, p, n)
        call calc_delta_t(g, p, n)

        ! boucle temporelle pour avoir C a chaque pas de temps
        time = 0.
        tf = (n%nb_ite-1) * n%dt
        Step = 1

        do i = 1, n%nb_ite
            call writer(n, g, noeud, time, tf, Step, dossier)
            call calc_c_t_dt(C_futur, g, p, n)

            call esperance(Stat, g%c, n, i)
            call variance(Stat, g%c, n, i)
            if (i > 1) then
                call covariance(Stat, g%c, C_futur, Stat%E(i-1), Stat%E(i), n, i)
            end if

            g%c = C_futur
            time = time + n%dt
            Step = Step + 1
        end do

        print*, "la concentration moyenne est constante des t = 0s, C_moy =", Stat%E(n%nb_ite)
        Stat%i_conv_var = 0
        Stat%i_conv_var = 0


        do i = 2, n%nb_ite
            if (abs(Stat%var(i)/Stat%var(i-1) -1.) < Stat%epsilon) then
                print*, "convergence de la variance à t =", i * n%delta_t/1000, "ms, iteration i =", i
                Stat%i_conv_var = i
                exit
            end if 
        end do
        do i = 2, n%nb_ite
            if (abs(Stat%Covar(i)/Stat%Covar(i-1) -1.) < Stat%epsilon) then
                print*, "convergence de la covariance à t =", i * n%delta_t/1000, "ms, iteration i =", i
                Stat%i_conv_covar = i
                exit
            end if 
        end do
        if (Stat%i_conv_var > 0 .AND. Stat%i_conv_covar >0) then 
            Stat%conv_pourcentage = 100. * (1. - Stat%Var(max(Stat%i_conv_var, Stat%i_conv_covar)) / Stat%Var(1))
            print*, "la solution stationnaire est atteinte à ", Stat%conv_pourcentage,&
             "% à l'itération ", max(Stat%i_conv_var, Stat%i_conv_covar)
        end if
    end if


    if (mode == 'convergence_maillage') then 
        n%nb_ite = 100
        nx_values = [40, 100, 300, 600, 1000]
        ny_values = [20, 50, 150, 300, 500]

        do i_vol = 1, 5
            n%nx = nx_values(i_vol)
            n%ny = ny_values(i_vol)

            !initialisation des tableaux et les paramètres du problème
            deallocate(g%c)
            allocate(g%c(n%nx, n%ny))
            deallocate(C_futur)
            allocate(C_futur(n%nx, n%ny))

            !initialisation des vitesses u et v
            deallocate(g%u)
            allocate(g%u(n%nx+1 , n%ny))
            deallocate(g%v)
            allocate(g%v(n%nx, n%ny+1))

            !Pour vts_writer
            deallocate(noeud%x)
            allocate(noeud%x(n%nx +1, n%ny +1))
            deallocate(noeud%y)
            allocate(noeud%y(n%nx +1, n%ny +1))
            call init_noeud(p, n, noeud)

            print*, "calcule pour un maillage de (nx,ny) =", n%nx, n%ny
            !initialisation de delta_x et delta_y, delta_t, C et des vitesses u et v
            n%delta_x = 2 * p%l / n%nx
            n%delta_y = p%l / n%ny
            call init_c(g%c, p, n)
            call init_v(g%u, g%v, p, n)
            call calc_delta_t(g, p, n)

            ! boucle temporelle pour avoir C a chaque pas de temps
            time = 0.
            tf = (n%nb_ite-1) * n%dt
            Step = 1
            call writer_python_convergence_maillage(n, p, g, 0, i_vol)
            do i = 1, n%nb_ite
                call writer(n, g, noeud, time, tf, Step, dossier)
                call writer_python_convergence_maillage(n, p, g, 1, i_vol)
                call calc_c_t_dt(C_futur, g, p, n)
                g%c = C_futur
                time = time + n%dt
                Step = Step + 1
            end do
        end do
    end if


    if (mode == 'peclet_diffusion') then
        n%nb_ite = 3000
        allocate(c_max(n%nb_ite))
        do i_pe = 1, 9
            n%pe = 10.**(i_pe - 3) ! pe allant de 0.01 à 10e6
            p%kappa = p%alpha*p%l/n%Pe
            print*, "calcul pour un nombre de Peclet de pe =", n%pe

            !initialisation de delta_x, delta_y, delta_t, C et des vitesses u et v
            n%delta_x = 2 * p%l / n%nx
            n%delta_y = p%l / n%ny
            call init_c(g%c, p, n)
            c_moy = sum(g%c) / (n%nx * n%ny)
            call init_v(g%u, g%v, p, n)
            call calc_delta_t(g, p, n)

            ! boucle temporelle pour avoir C a chaque pas de temps
            time = 0.
            tf = (n%nb_ite-1) * n%dt
            Step = 1
            do i = 1, n%nb_ite
                call writer(n, g, noeud, time, tf, Step, dossier)
                call calc_c_t_dt(C_futur, g, p, n)
                call calc_c_max(g, n, c_max, i)
                g%c = C_futur
                time = time + n%dt
                Step = Step + 1
            end do
            call write_python_peclet(n, p, c_max, i_pe)
        end do
    end if


    if (mode == 'peclet_advection') then
        n%nb_ite = 3000
        allocate(c_max(n%nb_ite))
        do i_pe = 1, 9
            n%pe = 10.**(i_pe - 3) ! pe allant de 0.01 à 10e6
            p%alpha = p%kappa/(p%l/n%Pe)
            print*, "calcul pour un nombre de Peclet de pe =", n%pe

            !initialisation de delta_x, delta_y, delta_t, C et des vitesses u et v
            n%delta_x = 2 * p%l / n%nx
            n%delta_y = p%l / n%ny
            call init_c(g%c, p, n)
            c_moy = sum(g%c) / (n%nx * n%ny)
            call init_v(g%u, g%v, p, n)
            call calc_delta_t(g, p, n)

            ! boucle temporelle pour avoir C a chaque pas de temps
            time = 0.
            tf = (n%nb_ite-1) * n%dt
            Step = 1
            do i = 1, n%nb_ite
                call writer(n, g, noeud, time, tf, Step, dossier)
                call calc_c_t_dt(C_futur, g, p, n)
                call calc_c_max(g, n, c_max, i)
                g%c = C_futur
                time = time + n%dt
                Step = Step + 1
            end do
            call write_python_peclet(n, p, c_max, i_pe)
        end do
    end if

end program main