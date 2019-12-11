! dummy file, used in case MPI is not available.

module autompi

use auto_constants, only: autoparameters

implicit none
private

public :: mpiini, mpiiap, mpiwfi, mpireduce, mpibcksub, mpisbv, mpicbv
public :: mpibcast, mpibcast1, mpibcasti, mpibcast1i, mpibcast1l, mpibcastap
public :: mpireducemax, mpireducemin
public :: mpigat, mpiscat, mpiend, mpitim, mpiiam, mpikwt, partition
public :: mpigats

contains

subroutine mpiini()
end subroutine mpiini

integer function mpiiam()
  mpiiam = 0
end function mpiiam

integer function mpikwt()
  mpikwt = 1
end function mpikwt

subroutine mpiiap(ap)
  type(autoparameters) :: ap
end subroutine mpiiap

logical function mpiwfi()
  mpiwfi = .false.
end function mpiwfi

subroutine partition(n,kwt,m)
  integer n,kwt,m(kwt)
  m(1) = n
end subroutine partition

subroutine mpireduce(a1,a2,bb,cc,c2,dd,faa,fcfc,ntst,nov,ncb,nrc,ifst,nllv,&
     lo,hi,level)
  integer, intent(in) :: ntst,nov,ncb,nrc,ifst,nllv,lo,hi,level
  double precision, intent(inout) :: a1(nov,nov,*),a2(nov,nov,*),bb(ncb,nov,*)
  double precision, intent(inout) :: cc(nov,nrc,*),c2(nov,nrc,*)
  double precision, intent(inout) :: dd(ncb,nrc,*),faa(nov,*)
  double precision, intent(inout) :: fcfc(nrc,*)
end subroutine mpireduce

subroutine mpibcksub(sol,fc,ntst,nov,ncb,lo,hi,level)
  integer, intent(in) :: ntst,nov,ncb,lo,hi,level
  double precision, intent(inout) :: sol(nov,*),fc(*)
end subroutine mpibcksub

subroutine mpisbv(solvbv)
  logical, intent(in) :: solvbv
end subroutine mpisbv

subroutine mpicbv(npar,par,rds)
  integer, intent(in) :: npar
  double precision :: par(npar)
  double precision :: rds
end subroutine mpicbv

subroutine mpibcast(buf,len)
  integer, intent(in) :: len
  double precision, intent(inout) :: buf(len)
end subroutine mpibcast

subroutine mpibcast1(buf)
  double precision, intent(inout) :: buf
end subroutine mpibcast1

subroutine mpibcasti(buf,len)
  integer, intent(in) :: len
  integer, intent(inout) :: buf(len)
end subroutine mpibcasti

subroutine mpibcast1i(buf)
  integer, intent(inout) :: buf
end subroutine mpibcast1i

subroutine mpibcast1l(buf)
  logical, intent(inout) :: buf
end subroutine mpibcast1l

subroutine mpibcastap(ap)
  type(autoparameters), intent(inout) :: ap
end subroutine mpibcastap

subroutine mpiscat(buf,ndx,n,add)
  integer, intent(in) :: ndx,n,add
  double precision, intent(inout) :: buf(ndx,*)
end subroutine mpiscat

subroutine mpigat(buf,ndx,n)
  integer, intent(in) :: ndx,n
  double precision, intent(inout) :: buf(ndx,*)
end subroutine mpigat

subroutine mpigats(buf,ndx,n,linelen)
  integer, intent(in) :: ndx,n,linelen
  character(linelen), intent(inout) :: buf(*)
end subroutine mpigats

subroutine mpireducemax(buf,n)
  integer, intent(in) :: n
  double precision, intent(inout) :: buf(n)
end subroutine mpireducemax

subroutine mpireducemin(buf,n)
  integer, intent(in) :: n
  double precision, intent(inout) :: buf(n)
end subroutine mpireducemin

subroutine mpiend()
end subroutine mpiend

subroutine mpitim(tim)
  double precision tim
end subroutine mpitim  

end module autompi
