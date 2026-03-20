subroutine ecriture(n, p, g, noeud, nb_ite)
    use m_type
    implicit none
    type(phys), intent(IN) :: p
    type(num), intent(IN) :: n
    type(grid), intent(IN) :: g
    type(noeud), intent(IN) :: noeud

    integer, intent(IN) :: nb_ite
    real :: Time
    integer :: Step, i,

    Step = n%dt
    Time = 0.
    call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, ini )

    Time = Time + real(Step)

    do i = 1, nb_ite-1
        call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, int)
        Time = Time + real(Step)
    end do

    call VTSWriter(Time, Step, n%nx, n%ny, noeud%x, noeud%y, g%c, g%u, g%v, end) 


	
end subroutine ecriture



