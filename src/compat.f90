! This file is only compiled if configure can't find flush or etime
! You can edit it to adjust these routines to the compiler that is used.

subroutine flush(i)
! The Fortran 2003 way:
! flush(i)
end subroutine flush

function etime(t)
  implicit none
  real t(2),etime
  t(1)=0
  t(2)=0
  etime=0
! The Fortran 95 way:
! call cputime(etime)
end function etime
