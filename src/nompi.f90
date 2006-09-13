! dummy file, used in case MPI is not available.

subroutine mpiini()
end subroutine mpiini

subroutine mpiiap(iap)
end subroutine mpiiap

subroutine mpiwfi(autobv,funi,icni)
end subroutine mpiwfi

subroutine mpicon(nov, na, nra, nca, a, ncb, b, nrc, c, d,irf, icf, comm_size)
  integer comm_size
  comm_size = 1
end subroutine mpicon

subroutine mpisbv(ndim,na,ncol,nint,ncb,nrc,nra,nca,ndx,iap,rap,par,icp, &
     fa,fc,rldot,ups,uoldps,udotps,upoldp,dtm,thl,thu,wi,wp,wt,comm_size)
  integer comm_size
  comm_size = 1
end subroutine mpisbv

subroutine mpiend()
end subroutine mpiend
