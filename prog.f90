program main
    use m_type
    use sousprog
    implicit none
    type(noueur) :: noeud
    type(phys) :: p
    type(num) :: n
    type(grid) :: g_t, g_t_dt
    real :: d_t, delta_x, delta_y, Time

    integer :: nb_ite, i, j, t, Step

    call reader('donnees.dat', p, n)

    allocate(g_t%c(n%nx, n%ny))
    allocate(g_t%u(n%nx +1 , n%ny))
    allocate(g_t%v(n%nx, n%ny +1))

    allocate(g_t_dt%c(n%nx, n%ny))
    allocate(g_t_dt%u(n%nx +1 , n%ny))
    allocate(g_t_dt%v(n%nx, n%ny +1))

    allocate(noeud%x(n%nx +1, n%ny +1))
    allocate(noeud%y(n%nx +1, n%ny +1))


    call init_c(g_t%c, p, n)
    call init_v(g_t%u, g_t%v, p, n)
    call init_noeud(g_t, p, n, noeud)
    call delta_t(g_t, p, n, d_t)

    call init_v(g_t_dt%u, g_t_dt%v, p, n)
    call init_noeud(g_t_dt, p, n, noeud)
    call delta_t(g_t_dt, p, n, d_t)

    delta_x = p%l / real(n%nx)
    delta_y = p%l / real(n%ny)

!    print*, n%nx, n%ny, n%dt
!    print*, p%kappa
!    print*, g_t%c
!    print*, "c'est en cours" 
!    print*, g_t%u
!    print*, "c'est fini" 
!    print*, g_t%v
!    print*, "on print les coordonnées des noeuds"

    call writer(n, g_t, noeud, 'ini', Time, Step)
    nb_ite = 10

    do t = 1, nb_ite-1
        do i = 1, n%nx
            do j = 1, n%ny
                g_t_dt%c(i,j) = g_t%c(i,j) + (d_t/(delta_x*delta_y))*(diffusion(i, j, g_t%c, p, n, d_t) +&
                 advection(i, j, g_t, n, p, delta_x, delta_y))
            end do
        end do
        call writer(n, g_t_dt, noeud, 'int', Time, Step)
        do i = 1, n%nx
            do j = 1, n%ny
                g_t%c(i,j) = g_t_dt%c(i,j)
            end do
        end do
    end do
    call writer(n, g_t_dt, noeud, 'end', Time, Step)
end program main