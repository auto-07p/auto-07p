module compat

implicit none
public :: autoflush, autim, autargc, autgetarg

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

    real t(2),etime
    autim = etime(t)

  end function autim

  integer function autargc()
    integer iargc
    autargc = iargc()
  end function autargc

  subroutine autgetarg(number,value)
    integer, intent(in) :: number
    character(len=*), intent(out) :: value
    call getarg(number,value)
  end subroutine autgetarg

end module compat
