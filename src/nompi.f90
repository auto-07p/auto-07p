! dummy file, used in case MPI is not available.

subroutine mpiini()
end subroutine mpiini

subroutine mpiiap(iap)
  integer iap(*)
  iap(39) = 1 ! KWT
end subroutine mpiiap

subroutine mpiwfi(autobv,funi,icni)
end subroutine mpiwfi

subroutine mpicon(a1,a2,bb,cc,d,faa,fc,na,nov,ncb,nrc,ifst,comm_size)
end subroutine mpicon

subroutine mpisbv(ndim,na,ncol,nint,ncb,nrc,nra,nca,ndx,iap,rap,par,icp, &
     rldot,rlold,ups,uoldps,udotps,upoldp,dtm,thu,ifst,nllv,comm_size)
end subroutine mpisbv

subroutine mpiinf(a,b,fa,sol,fc,na,nov,nra,nca,ncb,irf,icf,comm_size)
end subroutine mpiinf

subroutine mpiend()
end subroutine mpiend
