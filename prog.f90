program main
    use m_type
    use sousprog
    implicit none
    type(noueur) :: noeud
    type(phys) :: p
    type(num) :: n
    type(grid) :: g
    type(statistique) :: Stat
    real, dimension(:,:), allocatable :: C_futur, C_precedent
    real :: delta_t, time, Vol, Tf
    character(len=256), dimension(5) :: modes_list
    character(len=256) :: mode
    character(len=256) :: dossier
    logical :: found

    integer :: i, Step

    ! recupere le nom du dossier ou on veut stocker les resultats 
    call get_command_argument(1, dossier)
    dossier = trim(dossier)

    ! recupere le mode de calcul et test s'il est legitime
    modes_list(1) = 'classique'
    modes_list(2) = 'advection_pure_verticale'
    modes_list(3) = 'advection_pure_horizontale'
    modes_list(4) = 'diffusion_pure_verticale'
    modes_list(5) = 'diffusion_pure_horizontale'    
    call get_command_argument(2, mode)
    mode = trim(mode)
    found = .false.
    do i = 1, size(modes_list)
        if (trim(modes_list(i)) == trim(mode)) then
            found = .true.
            exit
        end if
    end do
    if (.not. found) then
        print *, "argument 2 (mode) must be one of the following: "
        do i = 1, size(modes_list)
            print *, " - ", trim(modes_list(i))
        end do
    end if


    !lit les données du problème
    call reader('donnees.dat', p, n)

    !initialise les tableaux et les paramètres du problème
    allocate(g%c(n%nx, n%ny))
    allocate(C_futur(n%nx, n%ny))
    allocate(C_precedent(n%nx, n%ny))


    allocate(g%u(n%nx +1 , n%ny))
    allocate(g%v(n%nx, n%ny +1))

    allocate(noeud%x(n%nx +1, n%ny +1))
    allocate(noeud%y(n%nx +1, n%ny +1))


    allocate(Stat%E(n%nb_ite))
    allocate(Stat%Var(n%nb_ite))
    allocate(Stat%Covar(n%nb_ite))


    call init_v(g%u, g%v, p, n)
    call init_noeud(p, n, noeud)
    C_precedent = g%c


    if (mode == 'classique') then

        vol = 2* p%l/n%nx * p%l / n%ny
        call init_c(g%c, p, n)
        
        call calc_delta_t(g, p, n, delta_t)

        ! boucle temporelle pour avoir C a chaque pas de temps
        time = 0.
        tf = (n%nb_ite-1) * n%dt
        Step = 1
        open(5, file="les resultats de statistiques")

        do i = 1, n%nb_ite
            call writer(n, g, noeud, time, tf, Step, dossier)
            call calc_c_t_dt(C_futur, g, delta_t, vol, p, n)
            g%c = C_futur
            time = time + n%dt
            Step = Step + 1

            g%c = C_futur


            call esperance(Stat, C_precedent, n, i)
            call variance(Stat, C_precedent, n, i)

            if (i > 1) then
                call covariance(Stat, C_precedent, g%c, Stat%E(i-1), Stat%E(i), n, i)
                write(5, *) Stat%E(i), Stat%Var(i), Stat%covar(i)

            end if
            C_precedent = g%c

        end do

            if (mode == 'verticale') then
        vol = 2* p%l/n%nx * p%l / n%ny
        call init_c_verticale(g%c, p, n)
        
        call calc_delta_t(g, p, n, delta_t)

        ! boucle temporelle pour avoir C a chaque pas de temps
        time = 0.
        tf = (n%nb_ite-1) * n%dt
        Step = 1

        do i = 1, n%nb_ite
            call writer(n, g, noeud, time, tf, Step, dossier)
            call calc_c_t_dt(C_futur, g, delta_t, vol, p, n)
            g%c = C_futur
            time = time + n%dt
            Step = Step + 1
        end do
    end if


    if (mode == 'horizontale') then
        vol = 2* p%l/n%nx * p%l / n%ny
        call init_c_horizontale(g%c, p, n)
        
        call calc_delta_t(g, p, n, delta_t)

        ! boucle temporelle pour avoir C a chaque pas de temps
        time = 0.
        tf = (n%nb_ite-1) * n%dt
        Step = 1

        do i = 1, n%nb_ite
            call writer(n, g, noeud, time, tf, Step, dossier)
            call calc_c_t_dt(C_futur, g, delta_t, vol, p, n)
            g%c = C_futur
            time = time + n%dt
            Step = Step + 1
        end do
    end if

        close(5)
    end if



end program main