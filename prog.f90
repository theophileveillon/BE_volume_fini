program main
    use m_type
    implicit none
    type(phys) :: phys
    type(num) :: num 
    type(amont) :: s
    type(centre) :: Cp
    type(centre) :: Cs
    type(centre) :: vite
    type(centre) :: s

    integer :: nombre_dite, i


    call lecture('data_2D.dat', p, n)

    allocate(s%x_mesh(n%nx))
    allocate(cp%C(n%nx))
    allocate(cf%C(n%nx))


    call mesh(s, p, n)

    call init(s, cp, cf, p, n)

   
    
    do i = 1, 100
        call ecriture(n, cf, s)
        call calc(cp, cf, n, p)
    end do

    

end program main



hgfdsgfdsgfds