module sousprog
use m_type
implicit none
contains

    subroutine reader(file_name, p, n)
        
        character (len = 11), intent(IN) :: file_name
        type(phys), intent(INOUT) :: p
        type(num), intent(INOUT) :: n

        open(10, file = file_name)

        read(10, *)  p%alpha
        read(10, *)  p%c0
        read(10, *)  p%kappa
        read(10, *)  p%l
        read(10, *)  p%d
        read(10, *)  n%nx
        read(10, *)  n%ny
        read(10, *)  n%dt
        read(10, *)  n%CLF
        read(10, *)  n%R
        read(10, *)  n%nb_ite

        close(10)
        
    end subroutine reader

    subroutine writer(n, g, noeud, Time, Tf, Step, dossier)
        
        !VTSWriter legerement modifier pour pouvoir mettre les .vts dans un dossier
        type(num), intent(IN) :: n
        type(grid), intent(IN) :: g
        type(noueur), intent(IN) :: noeud
        character(len=256), intent(IN) :: dossier

        real, intent(IN) :: Time, Tf
        integer, intent(IN) :: Step

        !pour l'affichage
        integer :: nbar, nfilled, pct
        character(len=50) :: bar
        nbar = len(bar)

        if (Time == 0.) then
            call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, 'ini', dossier)
        else
            pct     = int(100.0*Time/Tf + 0.5)
            nfilled = int(real(pct)/100.0 * nbar)
            bar     = repeat("=", nfilled)//repeat(" ", nbar-nfilled)
            write(*,'(a,1x,a," ",i3,"%",a)', advance='no') &
                    achar(13), "["//bar//"]", pct, ""
            call flush(6)
            if (abs(Time - Tf) < 1.e-6*Tf) then
                call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, 'end', dossier)
                write(*,*)
            else
                call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, 'int', dossier)
            end if
        end if
    end subroutine writer

    subroutine writer_x_L_2(n, p, g, init)

        type(num), intent(IN) :: n
        type(phys), intent(IN) :: p
        type(grid), intent(IN) :: g
        integer, intent(IN) :: init

        integer :: j

        if (init == 0) then
            open(9, file = 'x_L_2.dat', status = 'unknown')
            write(9, *) n%nx, n%ny, p%L/2, p%L, n%dt, n%nb_ite, p%kappa
            close(9)
        else    
            open(9, file = 'x_L_2.dat', status = 'old', position = 'append')
            do j = 1, n%ny
                write(9, '(E12.5)', advance="no") g%c(n%nx/2, j)
                if (j < n%ny) then
                    write(9, '(A)', advance="no") ';'
                end if
            end do
            write(9, *)
            close(9)
        end if

    end subroutine writer_x_L_2

    subroutine writer_y_L(n, p, g, init)

        type(num), intent(IN) :: n
        type(phys), intent(IN) :: p
        type(grid), intent(IN) :: g
        integer, intent(IN) :: init

        integer :: i

        if (init == 0) then
            open(9, file = 'y_L.dat', status = 'unknown')
            write(9, *) n%nx, n%ny, p%L/2, p%L, n%dt, n%nb_ite, p%kappa
            close(9)
        else    
            open(9, file = 'y_L.dat', status = 'old', position = 'append')
            do i = 1, n%nx
                write(9, '(E12.5)', advance="no") g%c(i, n%ny/2)
                if (i < n%nx) then
                    write(9, '(A)', advance="no") ';'
                end if
            end do
            write(9, *)
            close(9)
        end if

    end subroutine writer_y_L

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

    subroutine init_c_horizontale(c ,p ,n)
        
        real, dimension(:,:), intent(INOUT) :: c
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        integer :: i, j

        do i=1,n%nx
            do j=1,n%ny 
                if (j < n%ny/2) then
                    c(i,j) = c1(p%c0)
                else
                    c(i,j) = p%c0
                end if
            end do
        end do
    end subroutine init_c_horizontale

    subroutine init_c_verticale(c ,p ,n)
        
        real, dimension(:,:), intent(INOUT) :: c
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        integer :: i, j

        do i=1,n%nx
            do j=1,n%ny 
                if (i < n%nx/2) then
                    c(i,j) = c1(p%c0)
                else
                    c(i,j) = p%c0
                end if
            end do
        end do
    end subroutine init_c_verticale

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

    subroutine init_noeud(p, n, noeud)
        
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        type(noueur), intent(INOUT) :: noeud
        integer :: i, j
        
        do i=1,n%nx
            do j=1,n%ny 
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

        real :: f_delta_t, dx, dy
        type(grid), intent(IN) :: g
        integer, intent(IN) :: i, j
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n

        dx = 2. * p%l / real(n%nx)
        dy = p%l / real(n%ny)

        f_delta_t = abs(g%u(i, j)) / (real(n%CLF * 2) * dx) + abs(g%v(i, j)) / (real(n%CLF) * dy) +&
        p%kappa /(n%R * (dx)**2) + p%kappa / (n%R * (dy)**2)
    end function f_delta_t


    subroutine calc_delta_t(g, p, n, d_t)
        
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
    end subroutine calc_delta_t


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

        !condition est ouest
        if (i > 1) then
            if (g%u(i,j) > 0) then 
                qeo = qeo + Sy * g%u(i,j) * g%C(i-1,j)
            else 
                qeo = qeo + Sy * g%u(i,j) * g%C(i,j)
            end if
        end if


        if (i < n%nx) then
            if (g%u(i+1,j) > 0) then 
                qeo = qeo - Sy * g%u(i+1,j) * g%C(i,j)
            else 
                qeo = qeo- Sy * g%u(i+1,j) * g%C(i+1,j)
            end if
        end if

        !condition nord sud
        if (j >1) then
            if (g%v(i,j) > 0) then
                qns = qns + Sx * g%v(i,j) * g%c(i,j-1)
            else
                qns = qns + Sx * g%v(i,j) * g%c(i,j)
            end if
        end if

        if (j < n%ny) then
            if (g%v(i,j+1) > 0) then
                qns = qns-Sx * g%v(i,j+1) * g%c(i,j)
            else 
                qns = qns-sX * g%v(i,j+1) * g%c(i,j+1)
            end if
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

        !condition est ouest
        if (i > 1) then
            qeo = qeo + Sy * ( g%C(i-1,j) - g%C(i,j) )/Sx
        end if
        if (i < n%nx) then 
            qeo = qeo + Sy * (g%c(i+1,j) - g%C(i,j))/Sx
        end if
        
        !condition nord sud
        if (j > 1) then
            qns = qns +Sx * ( g%c(i, j-1) - g%c(i,j))/Sy
        end if 
        if (j < n%ny) then   
            qns = qns + Sx * (g%c(i, j+1) - g%c(i,j))/Sy
        end if

        diffusion = qeo + qns
    end function diffusion


    subroutine calc_c_t_dt(c_t_dt, g, delta_t, Vol, p, n)
        
        real, dimension(:,:), intent(OUT) :: c_t_dt
        type(grid), intent(IN) :: g
        type(phys), intent(IN) :: p
        type(num), intent(IN) :: n
        real, intent(IN) :: delta_t, Vol 
        integer :: i, j

        do i=1,n%nx
            do j=1,n%ny
                    c_t_dt(i,j) = g%C(i,j) + delta_t/Vol * (advection(g, n, p, i, j) + p%kappa * diffusion(g, n, p, i, j))
                end do
        end do

    end subroutine calc_c_t_dt

end module sousprog