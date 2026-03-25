module sousprog
use m_type
implicit none
contains

    subroutine reader(file_name, p, n)
        character (len = 11), intent(IN) :: file_name
        type(phys), intent(INOUT) :: p
        type(num), intent(INOUT) :: n

        open(10, file = file_name)

        read(10, *)   p%alpha
        read(10, *)  p%c0
        read(10, *)     p%kappa
        read(10, *)  p%l
        read(10, *)  p%d
        read(10, *)  n%nx
        read(10, *)  n%ny
        read(10, *)  n%dt
        read(10, *)  n%CLF
        read(10, *)  n%R

        close(10)
        
    end subroutine reader

    subroutine writer(n, p, g, noeud, nb_ite)
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        type(grid), intent(IN) :: g
        type(noueur), intent(IN) :: noeud

        integer, intent(IN) :: nb_ite
        real :: Time
        integer :: Step, i

        Step = int(n%dt)
        Time = 0.
        call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, 'ini' )

        Time = Time + n%dt
        print*, "on va essayer de rentrer dans la boucle"

        do i = 1, nb_ite-1
            call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, 'int')
            Time = Time + n%dt
            print*, "une étape c'est passé"
        end do
        print*, "la boucle est passé"
        call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, 'end') 

        print*, time 
        print*, step

    end subroutine writer

    function u(i, j, p, n)
        real :: u
        integer, intent(IN) :: i, j
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n 

        u = p%alpha * sin(acos(-1.0)*2*i/n%nx) * cos(acos(-1.0)*j/n%ny)
    end function u

    function v(i, j, p, n)
        real :: v
        integer, intent(IN) :: i, j
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n 
        v = -p%alpha * cos(acos(-1.0)*2*i/n%nx) * sin(acos(-1.0)*j/n%ny)
    end function v

    function c1(c0)
        real :: c1
        real, intent(IN) :: c0
        c1 = 1.-c0
    end function c1

    subroutine init_c(c ,p ,n)
        real, dimension(:,:), intent(INOUT) :: c
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        integer :: i, j
        real :: delta

        delta = 2./3. * real( min(2. * p%l/real(n%nx), p%l/real(n%ny)))

        do i=1,n%nx
            do j=1,n%ny 
                c(i,j)=c1(p%c0) +1./2. * (p%c0 - c1(p%c0)) * (1.+ erf(( 1./2. * sqrt( ( real(i) * p%l/real(n%nx) - p%l/2. )**2 +&
                (real(j) * p%l/(real(2*n%ny)) - p%l/4. )**2 ) -p%d/2. ) / delta ))
            end do
        end do
    end subroutine init_c

    subroutine init_v(u_g, v_g, p, n) 

        real, dimension(:,:), intent(INOUT) :: u_g, v_g
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        integer :: i, j

        do i=1,n%nx
            do j=1,n%ny 
                if ((i==1) .or. (i==n%nx)) then
                    u_g(i,j)=0
                else
                    u_g(i,j)=u(i,j,p, n)
                end if
                if((j==1) .or. (j==n%ny)) then
                    v_g(i,j)=0
                else
                    v_g(i,j)=v(i,j,p, n)
                end if
            end do             
        end do
    end subroutine init_v

    subroutine init_noeud(g, p, n, noeud)
        type(grid), intent(INOUT) :: g
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        type(noueur), intent(INOUT) :: noeud
        integer :: i, j
        
        do i=1,n%nx
            do j=1,n%ny 

    !            print*,  erf((sqrt( ( real(i) * real(n%nx) - p%L )**2 +&
    !             (real(j) * real(n%ny) - p%L/2. )**2 ) -p%d/2. ) / delta )

                noeud%x(i,j) = 2. *real(i-1) * p%L / real(n%nx) 
                noeud%y(i,j) = real(j-1) * p%L / real(n%ny) 
            end do
        end do

        do i = 1,  n%nx+1
            noeud%x(i, n%ny+1) = 2. * real(i-1) * p%L / real(n%nx) 
            noeud%y(i, n%ny+1) = p%L 

        end do
        do j = 1, n%ny+1 
            noeud%x(n%nx+1, j) = 2. * p%L
            noeud%y(n%nx+1,j) = real(j-1) * p%L / real(n%ny)  
        end do

    end subroutine init_noeud

    function f_delta_t(g ,i, j, p, n)

        real :: f_delta_t
        type(grid), intent(IN) :: g
        integer, intent(IN) :: i, j
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n

        f_delta_t = abs(g%u(i, j)) / (real(n%CLF * 2) * p%l / real(n%nx)) + abs(g%v(i, j)) / (real(n%CLF) * p%l / real(n%ny)) +&
        p%kappa /(n%R * (2. * p%l / real(n%nx))**2) + p%kappa / (n%R * ( p%l / real(n%ny))**2)
    end function f_delta_t


    subroutine delta_t(g, p, n, d_t) !obligé de faire une subroutine pcq blablabla j'ai pas compris

        type(grid), intent(INOUT) :: g
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        real, intent(OUT) :: d_t
        integer :: i, j
        real :: max_tmp
        max_tmp = 0;
        do i=1,n%nx
            do j=1,n%ny
                if (f_delta_t(g, i, j, p, n) > max_tmp) then
                    max_tmp = f_delta_t(g, i, j, p, n)
                end if
            end do
        end do
        d_t = 1. / max_tmp 
    end subroutine delta_t


    function advection(g, n, p, i, j)

        type(grid), intent(IN) :: g
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        real :: advection, qeo, qns, Sx, Sy
        integer, intent(IN) :: i, j
        Sx = 2* p%l/n%nx
        Sy = p%l / n%ny
        advection = 0.     

        qeo = 0
        qns = 0


        if (g%u(i,j) > 0) then
            qeo = qeo + Sy * g%u(i,j) * g%C(i-1,j)
        else 
            qeo = qeo+ Sy * g%u(i,j) * g%C(i,j)
        end if

        if (g%u(i+1,j) > 0) then 
            qeo = qeo - Sy * g%u(i+1,j) * g%C(i,j)
        else 
            qeo = qeo- Sy * g%u(i+1,j) * g%C(i+1,j)
        end if

        if (g%v(i,j) > 0) then
            qns = qns + Sx * g%v(i,j) * g%c(i,j-1)
        else
            qns = qns + Sx * g%v(i,j) * g%c(i,j)
        end if

        if (g%v(i,j+1) > 0) then
            qns = qns-Sx * g%v(i,j+1) * g%c(i,j)
        else 
            qns = qns-sX * g%v(i,j+1) * g%c(i,j+1)
        end if

        advection = qeo + qns


    end function advection

    function diffusion(g, n, p, i, j)

        type(grid), intent(IN) :: g
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        real :: diffusion, qeo, qns, Sx, Sy
        integer, intent(IN) :: i, j
        Sx = 2* p%l/n%nx
        Sy = p%l / n%ny
        diffusion = 0.
        qeo = 0
        qns = 0



            qeo = qeo +Sy * ( g%C(i-1,j) - g%C(i,j) )/Sx
            qeo = qeo +Sy * (g%c(i+1,j) - g%C(i,j))/Sx

            qns = qns +Sx * ( g%c(i, j-1) - g%c(i,j))/Sy
            qns =qns + Sx * (g%c(i, j+1) - g%c(i,j))/Sy


        diffusion = qeo + qns 

    end function diffusion


    !subroutine calc_c_t_dt(c_t_dt, c_t, delta_t, delta_x, delta_y, p, n) !obligé de faire plein de subroutine, une pour chaque terme pcq blablabla ils sont relou les profs
    !    real, dimension(:,:), intent(OUT) :: c_t_dt
    !    real, dimension(:,:), intent(IN) :: c_t
    !    type(phys), intent(IN) :: p
    !    type(num), intent(IN) :: n
    !    real, intent(IN) :: delta_t, delta_x, delta_y
    !    integer :: i, j
    !
    !    do i=1,n%nx
    !        do j=1,n%ny 
    !            if (u(i,j) > 0) then
    !                c_t_dt(i,j) = c_t(i,j) - delta_t/delta_x * u(i,j) * (c_t(i,j) - c_t(i-1,j)) + delta_t/delta_y * p%kappa * (c_t(i,j+1) - 2*c_t(i,j) + c_t(i,j-1))
    !            else
    !                c_t_dt(i,j) = c_t(i,j) + delta_t/delta_x * u(i,j) * (c_t(i+1,j) - c_t(i,j))
    !            end if
    !        end do
    !    end do
    !
    !end subroutine c_t_dt

end module sousprog