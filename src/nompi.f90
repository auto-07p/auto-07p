! dummy file, used in case MPI is not available.

module autompi

use auto_constants, only: autoparameters

implicit none
private
public :: mpiini, mpiiap, mpiwfi, mpireduce, mpibcksub, mpisbv, mpicbv, mpibcast
public :: mpibcasti, mpibcast1i, mpibcastap
public :: mpiadapt, mpigat, mpiend, mpitim, mpiiam, mpikwt, partition

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

subroutine mpireduce(a1,a2,bb,cc,c2,dd,faa,fcfc,ntst,nov,ncb,nrc,ifst,nllv,&
     lo,hi)
  integer, intent(in) :: ntst,nov,ncb,nrc,ifst,nllv,lo,hi
  double precision, intent(inout) :: a1(nov,nov,*),a2(nov,nov,*),bb(ncb,nov,*)
  double precision, intent(inout) :: cc(nov,nrc,*),c2(nov,nrc,*)
  double precision, intent(inout) :: dd(ncb,nrc,*),faa(nov,*)
  double precision, intent(inout) :: fcfc(nrc,*)
end subroutine mpireduce

subroutine mpibcksub(sol,ntst,nov,lo,hi)
  integer, intent(in) :: ntst,nov,lo,hi
  double precision, intent(inout) :: sol(nov,*)
end subroutine mpibcksub

subroutine mpisbv(ap,par,icp,ndim,uoldps,rds,rlold,rldot, &
     udotps,upoldp,dtm,thu,nllv)
  type(autoparameters) :: ap
  integer, intent(in) :: ndim,icp(*)
  integer, intent(inout) :: nllv
  double precision :: par(*),dtm(*),thu(*)
  double precision :: uoldps(ndim,0:*),udotps(ndim,0:*),upoldp(ndim,0:*)
  double precision :: rds,rlold(ap%nfpr),rldot(ap%nfpr)
end subroutine mpisbv

subroutine mpiadapt(ntst,ncol,ndim,ups,uoldps,dtm)
  integer, intent(in) :: ntst,ncol,ndim
  double precision :: ups(ndim,0:*),uoldps(ndim,0:*),dtm(0:*)
end subroutine mpiadapt

subroutine mpicbv(npar,par,rds,ss)
  integer, intent(in) :: npar
  double precision :: par(npar)
  double precision :: rds,ss
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

subroutine mpiend()
end subroutine mpiend

subroutine mpitim(tim)
  double precision tim
end subroutine mpitim  

subroutine partition(n,kwt,m)
  integer n,kwt,m(kwt)
  m(1) = n
end subroutine partition

end module autompi
