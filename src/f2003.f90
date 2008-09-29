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

  integer function autargc()
    autargc = command_argument_count()
  end function autargc

  subroutine autgetarg(number,value)
    integer, intent(in) :: number
    character(len=*), intent(out) :: value
    call get_command_argument(number,value)
  end subroutine autgetarg

end module compat
