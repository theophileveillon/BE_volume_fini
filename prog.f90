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

    ! recupere le nom du dossier ou on veut stocker les resultats 
    character(len=256) :: dossier
    call get_command_argument(1, dossier)   ! 1er argument après le nom du prog
    dossier = trim(dossier)

    !lit les données du problème
    call reader('donnees.dat', p, n)

    !initialise les tableaux et les paramètres du problème
    allocate(g%c(n%nx, n%ny))
    allocate(C_futur(n%nx, n%ny))

    allocate(g%u(n%nx +1 , n%ny))
    allocate(g%v(n%nx, n%ny +1))

    allocate(noeud%x(n%nx +1, n%ny +1))
    allocate(noeud%y(n%nx +1, n%ny +1))

    vol = 2* p%l/n%nx * p%l / n%ny
    call init_c(g%c, p, n)
    call init_v(g%u, g%v, p, n)
    call init_noeud(p, n, noeud)
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

end program main