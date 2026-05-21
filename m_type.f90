MODULE m_type
	implicit none

	Type phys
		real :: kappa, l, d, alpha, c0
	End Type phys


	Type noueur
		real, dimension(:,:), allocatable :: x, y
	end type noueur


	Type num
		integer :: nx, ny, nb_ite
		real :: CLF, dt, R, delta_x, delta_y, delta_t, pe
	End Type num

	Type grid
		real, dimension(:,:), allocatable :: c, u, v
	end type grid

	Type statistique
		real, dimension(:), allocatable :: E, Var, Covar
		real :: epsilon, conv_pourcentage
		integer :: i_conv_var, i_conv_covar
	end type statistique

END MODULE m_type