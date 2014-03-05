! dummy file, used in case MPI is not available.

module autompi

use auto_constants, only: autoparameters

implicit none
private

public :: mpiini, mpiiap, mpiwfi, mpireduce, mpibcksub, mpisbv, mpicbv, mpibcast
public :: mpibcasti, mpibcast1i, mpibcastap, mpireducemax
public :: mpiadapt, mpigat, mpiscat, mpiend, mpitim, mpiiam, mpikwt, partition

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

logical function mpiwfi(autobv)
  logical :: autobv
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

subroutine mpiadapt(ntst,ncol,ndim,ups,dtm)
  integer, intent(in) :: ntst,ncol,ndim
  double precision :: ups(ndim,0:*),dtm(0:*)
end subroutine mpiadapt

subroutine mpicbv(npar,par,nfpr,rldot,rds,ntst,ncol,ndim,udotps)
  integer, intent(in) :: npar,nfpr,ntst,ncol,ndim
  double precision :: udotps(ndim,0:*)
  double precision :: par(npar),rldot(nfpr)
  double precision :: rds
end subroutine mpicbv

subroutine mpibcast(buf,len)
  integer, intent(in) :: len
  double precision, intent(inout) :: buf(len)
end subroutine mpibcast

subroutine mpibcasti(buf,len)
  integer, intent(in) :: len
  integer, intent(inout) :: buf(len)
end subroutine mpibcasti

subroutine mpibcast1i(buf)
  integer, intent(inout) :: buf
end subroutine mpibcast1i

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

subroutine mpireducemax(buf,n)
  integer, intent(in) :: n
  double precision, intent(inout) :: buf(n)
end subroutine mpireducemax

subroutine mpiend()
end subroutine mpiend

subroutine mpitim(tim)
  double precision tim
end subroutine mpitim  

end module autompi
