program main
    use m_type
    use sousprog
    implicit none
    type(noueur) :: noeud
    type(phys) :: p
    type(num) :: n
    type(grid) :: g
    real, dimension(:,:), allocatable :: C_futur
    real :: delta_t, time, Vol, Tf
    integer :: i, Step

    !pour le makefile
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


    !lit les données du problème
    call reader('donnees.dat', p, n)

    !initialise les tableaux et les paramètres du problème
    allocate(g%c(n%nx, n%ny))
    allocate(C_futur(n%nx, n%ny))

    allocate(g%u(n%nx +1 , n%ny))
    allocate(g%v(n%nx, n%ny +1))

    allocate(noeud%x(n%nx +1, n%ny +1))
    allocate(noeud%y(n%nx +1, n%ny +1))

    call init_v(g%u, g%v, p, n)
    call init_noeud(p, n, noeud)

    if (mode == 'classique') then

        vol = 2* p%l/n%nx * p%l / n%ny
        call init_c(g%c, p, n)
        
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




end program main