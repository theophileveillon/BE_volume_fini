program main
    use m_type
    use sousprog
    implicit none
    type(noueur) :: noeud
    type(phys) :: p
    type(num) :: n
    type(grid) :: g
    real :: d_t

    integer :: nb_ite, i, j



    call reader('donnees.dat', p, n)

    allocate(g%c(n%nx, n%ny))
    allocate(g%u(n%nx +1 , n%ny))
    allocate(g%v(n%nx, n%ny +1))

    allocate(noeud%x(n%nx +1, n%ny +1))
    allocate(noeud%y(n%nx +1, n%ny +1))


    call init_c(g%c, p, n)
    call init_v(g%u, g%v, p, n)
    call init_noeud(g, p, n, noeud)
    call delta_t(g, p, n, d_t)
!    print*, n%nx, n%ny, n%dt
!    print*, p%kappa
!    print*, g%c
!    print*, "c'est en cours" 
!    print*, g%u
!    print*, "c'est fini" 
!    print*, g%v
!    print*, "on print les coordonnées des noeuds"

    do i = 1, n%nx +1
        do j = 1, n%ny +1
!            print*, "pour x", noeud%x(i,j)
!            print*, "pour y", noeud%y(i,j) 
        end do
    end do

    call writer(n, p, g, noeud, 1)

    

end program main