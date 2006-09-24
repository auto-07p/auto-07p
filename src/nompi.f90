! dummy file, used in case MPI is not available.

subroutine mpiini()
end subroutine mpiini

subroutine mpiiap(iap)
  integer iap(*)
  iap(39) = 1 ! KWT
end subroutine mpiiap

subroutine mpiwfi(autobv,funi,icni)
  logical :: autobv
  external funi,icni
end subroutine mpiwfi

subroutine mpicon(a1,a2,bb,cc,d,faa,fc,ntst,nov,ncb,nrc,ifst)
  implicit double precision(a-h,o-z)
end subroutine mpicon

subroutine mpisbv(iap,rap,par,icp,rldot,nra,ups,uoldps,udotps,upoldp,dtm, &
     thu,ifst,nllv)
  implicit double precision(a-h,o-z)
end subroutine mpisbv

subroutine mpibcast(buf,len)
  implicit double precision(a-h,o-z)
end subroutine mpibcast

subroutine mpiscat(buf,ndx,n,iadd)
  implicit double precision(a-h,o-z)
end subroutine mpiscat

subroutine mpigat(buf,ndx,n)
  implicit double precision(a-h,o-z)
end subroutine mpigat

subroutine mpiend()
end subroutine mpiend
