subroutine mesh(x_mesh, p, n)
	use m_type
	implicit none
	real :: pi
	
    type(phys), intent(IN) :: p
	type(num), intent(IN) :: n
    type(space), intent(INOUT) :: x_mesh
	integer :: i
	
	pi = acos(-1.)
	
	do i=1,n%nx
		x_mesh%x_mesh(i)=int(p%L)*(i-1)/(n%nx-1) + real(p%gamma)*p%L*sin(2.*pi*(i-1)/(n%nx-1)) / (3.*pi)

	end do

end subroutine mesh


function H(x)
    implicit none
    real :: H
    real, intent(IN) :: x
    if (x<0.) then
        H=0.0
    else
        H=1.0
    end if

end function H



function f(x,p)
    use m_type
    implicit none
    type(phys), intent(IN) :: p
    real :: f
	real :: H
    real, intent(IN) :: x
    f=H(x-p%L/2.+p%L/10.)-H(x-p%L/2.-p%L/10)
end function f


subroutine init(x_mesh, C1, C2, p, n)
    use m_type
    implicit none
    type(conc_pre), intent(INOUT) :: C1
	type(conc_futur), intent(INOUT) :: C2
    type(space), intent(IN) :: x_mesh
    type(phys), intent(IN) :: p
	type(num), intent(IN) :: n
    integer :: i
    real :: f
    do i=1,n%nx
        C1%C(i)=p%C0 * f(x_mesh%x_mesh(i),p)

        C2%C(i)=p%C0 * f(x_mesh%x_mesh(i),p)
    end do
	
end subroutine init

subroutine calc(C1,C2,n,p)
    use m_type
    implicit none
    type(conc_pre), intent(INOUT) :: C1
    type(conc_futur), intent(INOUT) :: C2
    type(num), intent(IN) :: n
    type(phys), intent(IN) :: p
	type(conc_futur) :: C3
    integer :: i

	allocate(C3%C(n%nx))

	do i = 1, n%nx
        C3%C(i)=C2%C(i)
	end do 

    do i=2,n%nx
        C2%C(i)=C1%C(i) - p%U0*n%dt*(C1%C(i)-C1%C(i-1))/(p%L/real((n%nx-1)))
    end do
    C2%C(1)=C2%C(n%nx)

	do i = 1, n%nx
        C1%C(i)=C3%C(i)
	end do 

    deallocate(C3%C)

end subroutine calc






subroutine lecture(texte, p, n)
	use m_type
	implicit none
	character (len = 11), intent(INOUT) :: texte
	type(phys), intent(INOUT) :: p
	type(num), intent(INOUT) :: n
	integer :: i


	open(1, file = texte)

	do i = 1, 2
		read(1,*)
	end do 

	read(1,*) p%L,        p%U0,       p%C0,     p%Gamma

	do i = 1, 9
		read(1,*)
	end do 

	read(1, *) n%nx, n%dt 
	
end subroutine lecture




subroutine ecriture(n, C2, s)
    use m_type
    implicit none
    type(num), intent(IN) :: n
    type(conc_futur), intent(IN) :: C2
    type(space), intent(IN) :: s
    integer :: i
    open(20, file='resultats.dat')
    do i=1, n%nx
        write(20, *) s%x_mesh(i) ,  C2%C(i)
    end do

	
    write(20,*) '--------------------'


end subroutine ecriture