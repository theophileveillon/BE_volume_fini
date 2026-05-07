MODULE m_type
	implicit none

	Type phys
		real :: kappa, l, d, alpha, c0
	End Type phys


	Type noueur
		real, dimension(:,:), allocatable :: x, y
	end type noueur


	Type num
		integer :: nx, ny, CLF, nb_ite
		real :: dt, R
	End Type num

	Type grid
		real, dimension(:,:), allocatable :: c, u, v
	end type grid



END MODULE m_type