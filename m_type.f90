MODULE m_type
	implicit none
	
	Type phys
		real :: kappa, L, D, alpha
	End Type phys
	
	Type num
		integer :: n 
		real :: dt
	End Type num

	Type centre
		real, dimension(:,:), allocatable :: val
	end type centre

	Type amont
		real, dimension(:), allocatable :: val
	end type amont


END MODULE m_type