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

    subroutine writer(n, p, g, noeud, Time, Tf, Step)
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        type(grid), intent(IN) :: g
        type(noueur), intent(IN) :: noeud

        real, intent(IN) :: Time, Tf
        integer, intent(IN) :: Step


        if (Time == 0.) then

            call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, 'ini' )
        else
            if (Time == Tf) then
                call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, 'end') 
            else
                call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, 'int')

            end if
        end if
            
        


        print*, time 
        print*, step

    end subroutine writer

    function u(i, j, p, n)
        real :: u
        integer, intent(IN) :: i, j
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n 

        u = p%alpha 
    end function u

    function v(i, j, p, n)
        real :: v
        integer, intent(IN) :: i, j
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n 
        v = 0
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
!                c(i,j)=c1(p%c0) +1./2. * (p%c0 - c1(p%c0)) * (1.+ erf(( 1./2. * sqrt( ( real(i) * p%l/real(n%nx) - p%l/2. )**2 +&
!                (real(j) * p%l/(real(2*n%ny)) - p%l/4. )**2 ) -p%d/2. ) / delta ))
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
                    u_g(i,j)=5
                else
                    u_g(i,j)=u(i,j,p, n)
                end if
                if((j==1) .or. (j==n%ny)) then
                    v_g(i,j)=5
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
            if (i >1) then
                qeo = qeo + Sy * g%u(i,j) * g%C(i-1,j)
            else 
                qeo = qeo + Sy * g%u(i,j) * 1
            end if
        else 
            qeo = qeo+ Sy * g%u(i,j) * g%C(i,j)
        end if


        if (i < n%nx) then
            if (g%u(i+1,j) > 0) then 
            
                qeo = qeo - Sy * g%u(i+1,j) * g%C(i,j)
            else 
                qeo = qeo- Sy * g%u(i+1,j) * g%C(i+1,j)
            end if
        else 
        
            qeo = qeo + Sy * g%u(i+1,j) * 1
        end if



        if (g%v(i,j) > 0) then
            if (j >1) then
                qns = qns + Sx * g%v(i,j) * g%c(i,j-1)
            else 
                qns = qns + Sx * g%v(i,j) * 1
            end if
        else
            qns = qns + Sx * g%v(i,j) * g%c(i,j)
        end if

        if (j < n%ny) then
            if (g%v(i,j+1) > 0) then
                qns = qns-Sx * g%v(i,j+1) * g%c(i,j)
            else 
                qns = qns-sX * g%v(i,j+1) * g%c(i,j+1)
            end if
        else
            qns = qns + Sx * g%v(i,j+1)
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


        if (i >1) then
            qeo = qeo +Sy * ( g%C(i-1,j) - g%C(i,j) )/Sx
        end if

        if (i < n%nx -1) then 
            qeo = qeo +Sy * (g%c(i+1,j) - g%C(i,j))/Sx
        end if
        
        if (j > 1) then
            qns = qns +Sx * ( g%c(i, j-1) - g%c(i,j))/Sy
        end if 
        if (j< n%ny - 1) then   
            qns =qns + Sx * (g%c(i, j+1) - g%c(i,j))/Sy
        end if

        diffusion = qeo + qns 

    end function diffusion


    subroutine calc_c_t_dt(c_t_dt, g, delta_t, Vol, p, n) !obligé de faire plein de subroutine, une pour chaque terme pcq blablabla ils sont relou les profs
        real, dimension(:,:), intent(OUT) :: c_t_dt
        type(grid), intent(IN) :: g
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        real, intent(IN) :: delta_t, Vol 
        integer :: i, j
    
        do i=1,n%nx
            do j=1,n%ny 
                    c_t_dt(i,j) = g%C(i,j) + delta_t/Vol * advection(g, n, p, i, j) + delta_t * p%kappa * diffusion(g, n, p, i, j)
            end do
        end do
    
    end subroutine calc_c_t_dt

end module sousprog