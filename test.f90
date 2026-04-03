program progress_demo
  implicit none
  real :: Time, Time_max, d_t
  integer :: i, nbar, nfilled, pct
  character(len=50) :: bar

  Time_max = 10.0
  d_t      = 0.1
  nbar     = len(bar)

  Time = 0.0
  do while (Time <= Time_max)
     pct = int(100.0*Time/Time_max + 0.5)

     ! nombre de caractères remplis
     nfilled = int(real(pct)/100.0 * nbar)

     bar = repeat("=", nfilled)//repeat(" ", nbar-nfilled)

     write(*,'(a,1x,a," ",i3,"%",a)', advance='no')  &
          achar(13), "["//bar//"]", pct, ""   ! achar(13) = retour chariot

     call flush(6)   ! ou flush(*), selon ton compilateur

     call sleep(1)   ! juste pour voir la barre évoluer (optionnel)
     Time = Time + d_t
  end do

  ! Saut de ligne à la fin
  write(*,*)
end program progress_demo