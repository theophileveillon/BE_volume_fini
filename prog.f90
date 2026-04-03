program main
    use m_type
    use sousprog
    implicit none
    type(noueur) :: noeud
    type(phys) :: p
    type(num) :: n

    type(grid) :: g
    real, dimension(:,:), allocatable :: C_futur
    real :: d_t, Time, Vol, Tf

    integer :: nb_ite, i, j, Step



    integer :: nb_ite, i, j, t, Step

    call reader('donnees.dat', p, n)


    allocate(g%c(n%nx, n%ny))
    allocate(C_futur(n%nx, n%ny))

    allocate(g%u(n%nx +1 , n%ny))
    allocate(g%v(n%nx, n%ny +1))


    allocate(noeud%x(n%nx +1, n%ny +1))
    allocate(noeud%y(n%nx +1, n%ny +1))

    Vol = 2* p%l/n%nx * p%l / n%ny
    call init_c(g%c, p, n)
    call init_v(g%u, g%v, p, n)
    call init_noeud(g, p, n, noeud)
    call delta_t(g, p, n, d_t)

!    print*, n%nx, n%ny, n%dt
!    print*, p%kappa
!    print*, g_t%c
!    print*, "c'est en cours" 
!    print*, g_t%u
!    print*, "c'est fini" 
!    print*, g_t%v
!    print*, "on print les coordonnées des noeuds"



    Time = 0.
    nb_ite = 1000
    Tf = (nb_ite-1) * n%dt
    Step = 1


    do i = 1, nb_ite
        call writer(n, p, g, noeud, Time, Tf, Step)
        call calc_c_t_dt(C_futur, g, d_t, Vol, p, n) 
        g%c = C_futur
        Time = Time + n%dt
        Step = Step + 1

    end do


    


end program main