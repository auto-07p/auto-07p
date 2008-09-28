module compat

implicit none
public :: autoflush, autim

contains

! This file calls Unix extensions (see f2003.f90 for the new Standard forms)

  subroutine autoflush(i)
    integer, intent(in) :: i
    call flush(i)
  end subroutine autoflush

!-----------------------------------------------------------------------
!          Timing AUTO
!-----------------------------------------------------------------------

! ------ --------- --------
  double precision function autim()

    real t(2)
    autim = etime(t)

  end function autim

end module compat
