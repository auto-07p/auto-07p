! dummy file, used in case MPI is not available.

module autompi

implicit none
private
public :: mpiini, mpiiap, mpiwfi, mpicon, mpisbv, mpibcast, mpibcasti, mpiscat
public :: mpigat, mpiend, mpitim, partition

contains

subroutine mpiini(iap)
  integer iap(*)
  iap(38) = 0 ! IAM
  iap(39) = 1 ! KWT
end subroutine mpiini

subroutine mpiiap(iap)
  integer iap(*)
end subroutine mpiiap

logical function mpiwfi(autobv)
  logical :: autobv
  mpiwfi = .false.
end function mpiwfi

subroutine mpicon(s1,a1,a2,bb,cc,d,faa,fc,ntst,nov,ncb,nrc,ifst)
  integer :: ntst, nov, ncb, nrc, ifst
  double precision :: a1(nov,nov,*),a2(nov,nov,*),bb(ncb,nov,*),cc(nov,nrc,*)
  double precision :: s1(nov,nov,*),d(ncb,*),faa(nov,*),fc(*)
end subroutine mpicon

subroutine mpisbv(iap,rap,par,icp,nra,ups,uoldps,udotps,upoldp,dtm, &
     thu,ifst,nllv)
  integer :: nra,iap(*),icp(*),ifst,nllv
  double precision :: rap(*),par(*),dtm(*),thu(*)
  double precision :: ups(nra,*),uoldps(nra,*),udotps(nra,*),upoldp(nra,*)
end subroutine mpisbv

subroutine mpibcast(buf,len)
  integer :: len
  double precision :: buf(len)
end subroutine mpibcast

subroutine mpibcasti(buf,len)
  integer :: len, buf(len)
end subroutine mpibcasti

subroutine mpiscat(buf,ndx,n,add)
  integer ndx,n,add
  double precision :: buf(ndx,*)
end subroutine mpiscat

subroutine mpigat(buf,ndx,n)
  integer ndx,n
  double precision :: buf(ndx,*)
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
