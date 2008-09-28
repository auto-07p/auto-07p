module compat

implicit none
public :: autoflush, autim

contains

! This file calls some F2003/F95 functions that were previously only
! available as Unix extensions.

  subroutine autoflush(i)
! The Fortran 2003 way:
    integer, intent(in) :: i
    flush(i)
  end subroutine autoflush

!-----------------------------------------------------------------------
!          Timing AUTO
!-----------------------------------------------------------------------

! ------ --------- --------
  double precision function autim()

    real etime

    call cpu_time(etime)
    autim = etime

  end function autim

end module compat
