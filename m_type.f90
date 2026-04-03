MODULE m_type
	implicit none

	Type phys
		real :: kappa, l, d, alpha, c0
	End Type phys


	Type noueur
		real, dimension(:,:), allocatable :: x, y
	end Type noueur


	Type num
		integer :: nx, ny, CLF
		real :: dt, R
	End Type num

	Type grid
		real, dimension(:,:), allocatable :: c, u, v
	end Type grid



END MODULE m_type