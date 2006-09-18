! This file contains all MPI specific functions in AUTO, both the
! master and workers side.
!
! External subroutines called from the main part of AUTO:
!
! mpiini: inits MPI, and the worker receives some constants
! mpiiap: the master sends constants so that the workers can determine
!         the funi and icni functions to use
! mpiwfi: main workers' loop, called by AUTOBV with funi and icni known
!         contains the client side of CONPAR and SETUBV   
! mpicon and mpisbv: master side of CONPAR and SETUBV
! mpiend: master tells workers to stop

subroutine mpiini()
  implicit none
  include 'mpif.h'

  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

  integer ierr,comm_size,myid,namelen
  character(len=MPI_MAX_PROCESSOR_NAME) processor_name

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,comm_size,ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD,myid,ierr)
  call MPI_Get_processor_name(processor_name,namelen,ierr)
!  print *,'Process ',myid,' on ',processor_name
  if(myid/=0)then
     call mpi_worker()
  endif

contains

  subroutine mpi_worker()
    implicit none
    include 'mpif.h'

    integer :: message_type, ierr, stat(MPI_STATUS_SIZE)
    integer :: funi_icni_params(5), iap(38), icp, iuz
    double precision :: rap,par,thl,thu,vuz

    call MPI_Recv(message_type,1,MPI_INTEGER,MPI_ANY_SOURCE,MPI_ANY_TAG, &
         MPI_COMM_WORLD,stat,ierr)
    if(message_type /= AUTO_MPI_INIT_MESSAGE)then
       print *,'Fatal: no init message, message received: ', message_type
       stop
    endif
    do while(.true.)
       call MPI_Bcast(funi_icni_params,5,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
       ! figure out what funi and icni are from
       ! the iap array.  This is originally done 
       ! in autlib1.f.  We do it here, since I
       ! don't know how to pass function pointers
       ! through MPI in a possibly heterogeneous 
       ! environment :-)
       iap(2)  = funi_icni_params(1) ! ips
       iap(3)  = funi_icni_params(2) ! irs
       iap(10) = funi_icni_params(3) ! isw
       iap(27) = funi_icni_params(4) ! itp
       iap(29) = funi_icni_params(5) ! nfpr
       iap(38) = 1                   ! iam
       call autoi(iap,rap,par,icp,thl,thu,iuz,vuz)
       ! autoi will call mpiwfi; a return means another init message
    enddo
  end subroutine mpi_worker

end subroutine mpiini

subroutine mpiiap(iap)
  implicit none
  include 'mpif.h'

  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

  integer :: iap(*)

  ! A few words about what is going on here.  ips, irs, isw, itp, and
  ! nfpr are used to choose which functions are used for funi, icni, bcni, etc.
  ! unfortunately, their values are changed in init1 and chdim.  In the
  ! old version of AUTO the functions were already choosen by the point
  ! these values were modified, so there was no problem.  Now, in the
  ! message passing parallel version, the workers need both versions, since
  ! they both need to select the appropriate functions (using the old values)
  ! and actually compute (using the new values).
  integer comm_size,i,ierr
  integer funi_icni_params(5)

  call MPI_Comm_size(MPI_COMM_WORLD,comm_size,ierr)
  iap(39)=comm_size

  funi_icni_params(1)=iap(2)  ! ips
  funi_icni_params(2)=iap(3)  ! irs
  funi_icni_params(3)=iap(10) ! isw
  funi_icni_params(4)=iap(27) ! itp
  funi_icni_params(5)=iap(29) ! nfpr
  do i=1,comm_size-1
     ! Send message to get worker into init mode
     call MPI_Send(AUTO_MPI_INIT_MESSAGE,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,ierr)
  enddo
  call MPI_Bcast(funi_icni_params,5,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
end subroutine mpiiap

subroutine mpiwfi(autobv,funi,icni)
  implicit none
  include 'mpif.h'

  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

  logical :: autobv
  external funi,icni

  integer, allocatable, save :: irf(:,:), icf(:,:)
  double precision, allocatable, save :: a(:,:,:), b(:,:,:), c(:,:,:), d(:,:)
  double precision, allocatable :: fa(:,:), fc(:)

  integer :: message_type, ierr, stat(MPI_STATUS_SIZE), na
  integer :: params(9)

  if (.not.autobv) then
     print *,'Illegal problem type for MPI'
     call MPI_Finalize(ierr)
     stop
  endif

  do while(.true.)
     call MPI_Recv(message_type,1,MPI_INTEGER,MPI_ANY_SOURCE,MPI_ANY_TAG, &
          MPI_COMM_WORLD,stat,ierr)
  
     select case(message_type)
     case(AUTO_MPI_KILL_MESSAGE) ! The kill message
        call MPI_Finalize(ierr)
        stop
     case(AUTO_MPI_INIT_MESSAGE)
        return
     case(AUTO_MPI_SETUBV_MESSAGE) ! The setubv message
        ! input scalars
        call MPI_Recv(na,1,MPI_INTEGER,MPI_ANY_SOURCE,MPI_ANY_TAG, &
             MPI_COMM_WORLD,stat,ierr)
        call MPI_Bcast(params,9,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        call mpi_setubv_worker(funi,icni,na,params)
        call mpi_conpar_worker(na,params)
     case default
        print *,'Unknown message recieved: ', message_type
     end select
  enddo

contains

  subroutine mpi_conpar_worker(na,params)
    implicit none
    include 'mpif.h'

    integer :: na, params(*)

    integer :: nov, nra, nca, ncb, nrc, ifst, nllv
    double precision, allocatable :: a1(:,:,:),a2(:,:,:),bb(:,:,:),cc(:,:,:)
    double precision, allocatable :: faa(:,:),sol(:,:)
    double precision dum

    ! input scalars
    nov = params(1)
    nra = params(2)
    nca = params(3)
    ncb = params(4)
    nrc = params(5)
    ifst = params(8)
    nllv = params(9)

    !input/output arrays

    allocate(a1(nov,nov,na),a2(nov,nov,na),bb(ncb,nov,na),cc(nov,nrc,na+1))
    allocate(faa(nov,na))

    if(ifst==1)then
       call conpar(nov,na,nra,nca,a,ncb,b,nrc,c,d,irf,icf)
       call copycp(na,nov,nra,nca,a,ncb,b,nrc,c,a1,a2,bb,cc,irf)
    endif

    if(nllv==0)then
       call conrhs(nov,na,nra,nca,a,nrc,c,fa,fc,irf,icf)
    else
       call setzero(fa,fc,na,nra,nrc)
    endif
    call cpyrhs(na,nov,nra,faa,fa,irf)

    call mpicon_comm(a1,a2,bb,cc,d,dum,faa,fc,dum,na,nov,ncb,nrc,ifst,0)

    deallocate(fc)
    allocate(fc(nov+ncb),sol(nov,na+1))
    call mpiinf(a,b,fa,sol,fc,na,nov,nra,nca,ncb,irf,icf,0)

    deallocate(sol,faa,a1,a2,bb,cc)

    deallocate(fa,fc)
  end subroutine mpi_conpar_worker

  subroutine mpi_setubv_worker(funi,icni,na,params)
    implicit none
    include 'mpif.h'
    integer NIAP,NRAP,NPARX,NBIFX
    include 'auto.h'
    integer, parameter :: NPARX2=2*NPARX

    integer :: na, params(9)

    integer :: ndim, ncol, nint, ncb, nrc, nra, nca, ifst
    integer :: i, j, ierr

    integer, allocatable :: iap(:),icp(:)
    double precision, allocatable :: rap(:), par(:), ups(:,:), uoldps(:,:)
    double precision, allocatable :: udotps(:,:), upoldp(:,:), thu(:), rldot(:)
    double precision, allocatable :: dtm(:)

    integer :: pos, bufsize, size_int, size_double
    integer, allocatable :: buffer(:)

    external funi, icni

    ndim=params(1)
    nra=params(2)
    nca=params(3)
    ncb=params(4)
    nrc=params(5)
    ncol=params(6)
    nint=params(7)
    ifst=params(8)

    allocate(iap(NIAP),rap(NRAP),par(NPARX2),icp(NPARX))
    allocate(thu(ndim*8),rldot(NPARX))

    ! Here we compute the number of elements in the iap and rap structures.
    ! Since each of the structures is homogeneous we just divide the total
    ! size by the size of the individual elements.

    call MPI_Pack_size(NIAP+NPARX,MPI_INTEGER,MPI_COMM_WORLD,size_int,ierr)
    call MPI_Pack_size(NRAP+NPARX2+ndim*8+NPARX, &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,size_double,ierr)
    bufsize = size_int + size_double
    allocate(buffer(bufsize))
    call MPI_Bcast(buffer  ,bufsize,MPI_PACKED,0,MPI_COMM_WORLD,ierr)

    pos = 0
    call MPI_Unpack(buffer,bufsize,pos,iap   ,NIAP, &
         MPI_INTEGER,MPI_COMM_WORLD,ierr)
    call MPI_Unpack(buffer,bufsize,pos,rap   ,NRAP, &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
    ! /***********************************/
    call MPI_Unpack(buffer,bufsize,pos,par   ,NPARX2, &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
    call MPI_Unpack(buffer,bufsize,pos,icp   ,NPARX, &
         MPI_INTEGER,MPI_COMM_WORLD,ierr)
    call MPI_Unpack(buffer,bufsize,pos,thu   ,ndim*8, &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
    call MPI_Unpack(buffer,bufsize,pos,rldot ,NPARX, &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
    deallocate(buffer)

    allocate(dtm(na))
    allocate(ups(nra,na+1),uoldps(nra,na+1),udotps(nra,na+1),upoldp(nra,na+1))
    call mpisbv_comm(na,nra,dtm,ups,uoldps,udotps,upoldp,0)

    ! output arrays

    if(ifst==1)then
       if(allocated(a))then
          deallocate(a,b,c,d,irf,icf)
       endif
       allocate(a(nca,nra,na),b(ncb,nra,na),c(nca,nrc,na),d(ncb,nrc))
       allocate(irf(nra,na),icf(nca,na))
    endif
    allocate(fa(nra,na),fc(nrc))

    ! A little explanation of what is going on here
    ! is in order I believe.  This array is
    ! created by a summation across all workers,
    ! hence it needs a summation in the master.
    !
    ! We sum into d, which is a local variable initialized to
    ! 0.0. We then sum our part with the masters part
    ! in the master.
    !
    ! zero pseudo-arclength part of array, rest is done in setubv()

    do i=nint+1,nrc
       fc(i)=0.0d0
       do j=1,ncb
          d(j,i)=0.0d0
       enddo
    enddo

    call setubv(ndim, na, ncol, nint, ncb, nrc, nra, nca, funi, icni, nra, &
         iap, rap, par, icp, a, b, c, d, fa, fc, ups, &
         uoldps, udotps, upoldp, dtm, thu, ifst)

    ! free input arrays
    deallocate(iap,rap,par,icp,ups,uoldps,dtm,udotps,upoldp,thu)
    deallocate(rldot)
  end subroutine mpi_setubv_worker

end subroutine mpiwfi

subroutine mpicon_comm(a1,a2,bb,cc,d,drec,faa,fc,fcrec,na,nov,ncb,nrc,ifst,&
     comm_size)
  implicit none
  include 'mpif.h'

  integer :: na, nov, ncb, nrc, ifst, comm_size
  double precision :: a1(nov,nov,*),a2(nov,nov,*),bb(ncb,nov,*),cc(nov,nrc,*)
  double precision :: d(ncb,*),drec(ncb,*),faa(nov,*),fc(*),fcrec(*)

  integer :: i,ii,j,k,n,ierr,loop_start,loop_end
  integer, allocatable :: a_counts(:),a_displacements(:)
  integer, allocatable :: b_counts(:),b_displacements(:)
  integer, allocatable :: c_counts(:),c_displacements(:)
  integer, allocatable :: faa_counts(:), faa_displacements(:)
  double precision, allocatable :: ccb(:,:,:)

  allocate(faa_counts(comm_size),faa_displacements(comm_size))
  do i=1,comm_size
     loop_start = ((i-1)*na)/comm_size
     loop_end = (i*na)/comm_size
     if(i==1)loop_end = 0
     faa_counts(i) = nov*(loop_end-loop_start)
     faa_displacements(i) = nov*loop_start
  enddo
  call MPI_Gatherv(faa,nov*na,MPI_DOUBLE_PRECISION, &
       faa,faa_counts,faa_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  deallocate(faa_counts,faa_displacements)
  call MPI_Reduce(fc,fcrec,nrc,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)
  if(ifst==0)return

  allocate(a_counts(comm_size),a_displacements(comm_size))
  allocate(b_counts(comm_size),b_displacements(comm_size))
  allocate(c_counts(comm_size),c_displacements(comm_size))
  allocate(ccb(nov,nrc,comm_size))

  do i=1,comm_size
     loop_start = ((i-1)*na)/comm_size
     loop_end = (i*na)/comm_size
     if(i==1)loop_end = 0
     a_counts(i) = nov*nov*(loop_end-loop_start)
     a_displacements(i) = nov*nov*loop_start
     b_counts(i) = nov*ncb*(loop_end-loop_start)
     b_displacements(i) = nov*ncb*loop_start
     c_counts(i) = nov*nrc*(loop_end-loop_start)
     c_displacements(i) = nov*nrc*loop_start
     if(i==comm_size)then
        c_counts(i) = c_counts(i) + nov*nrc
     endif
  enddo

  call MPI_Gatherv(a1, nov*nov*na,MPI_DOUBLE_PRECISION, &
       a1,a_counts,a_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  call MPI_Gatherv(a2, nov*nov*na,MPI_DOUBLE_PRECISION, &
       a2,a_counts,a_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  call MPI_Gatherv(bb, nov*ncb*na,MPI_DOUBLE_PRECISION, &
       bb,b_counts,b_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  call MPI_Gatherv(cc, nov*nrc*na,MPI_DOUBLE_PRECISION, &
       cc,c_counts,c_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  n=na
  if (comm_size/=0) n=na/comm_size
  call MPI_Gather(cc(1,1,n+1), nov*nrc,MPI_DOUBLE_PRECISION, &
       ccb,nov*nrc,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)

  do i=1,comm_size-1
     !fix up boundaries between cc parts
     ii = (i*na)/comm_size
     do j=1,nrc
        do k=1,nov
           cc(k,j,ii+1)=cc(k,j,ii+1)+cc(k,j,ii)
           cc(k,j,ii)=cc(k,j,ii)+ccb(k,j,i)
        enddo
     enddo
  enddo

  deallocate(a_counts,a_displacements,b_counts,b_displacements)
  deallocate(c_counts,c_displacements,ccb)

  call MPI_Reduce(d,drec,ncb*nrc,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

end subroutine mpicon_comm

subroutine mpicon(a1,a2,bb,cc,d,faa,fc,na,nov,ncb,nrc,ifst,comm_size)
  implicit none
  include 'mpif.h'

  integer :: na, nov, ncb, nrc, ifst, comm_size
  double precision :: a1(nov,nov,*),a2(nov,nov,*),bb(ncb,nov,*)
  double precision :: cc(nov,nrc,*),d(ncb,*),faa(nov,*),fc(*)

  integer i,j
  double precision, allocatable :: dtemp(:,:),fctemp(:)

  ! Worker is running now

  ! I create a temporary receive buffer for the MPI_Reduce
  ! command.  This is because there isn't an
  ! asymmetric version (like MPI_Scatterv).
  allocate(dtemp(ncb,nrc),fctemp(nrc))

  call mpicon_comm(a1,a2,bb,cc,d,dtemp,faa,fc,fctemp,na,nov,ncb,nrc,ifst,&
       comm_size)

  do i=1,comm_size
     
  enddo

  if(ifst==1)then
     do i=1,nrc
        do j=1,ncb
           d(j,i)=dtemp(j,i)
        enddo
     enddo
  endif
  do i=1,nrc
     fc(i)=fctemp(i)
  enddo
  deallocate(dtemp,fctemp)

end subroutine mpicon

subroutine mpisbv_comm(na,nra,dtm,ups,uoldps,udotps,upoldp,comm_size)
  implicit none

  include 'mpif.h'

  integer :: na, nra, comm_size
  double precision :: dtm(*),ups(nra,*),uoldps(nra,*),udotps(nra,*)
  double precision :: upoldp(nra,*)

  integer :: i,loop_start,loop_end,ierr
  integer, allocatable :: dtm_counts(:), dtm_displacements(:)
  integer, allocatable :: ups_counts(:), ups_displacements(:)

  allocate(dtm_counts(comm_size),dtm_displacements(comm_size))
  allocate(ups_counts(comm_size),ups_displacements(comm_size))

  do i=1,comm_size
    loop_start = ((i-1)*na)/comm_size
    loop_end = (i*na)/comm_size
    dtm_counts(i) = loop_end-loop_start
    dtm_displacements(i) = loop_start
    ups_counts(i) = nra*(loop_end-loop_start+1)
    ups_displacements(i) = nra*loop_start
  enddo
  if(comm_size>0)then
     dtm_counts(1) = 0
     ups_counts(1) = 0
  endif

  call MPI_Scatterv(dtm,dtm_counts,dtm_displacements,MPI_DOUBLE_PRECISION, &
       dtm,na,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_Scatterv(ups,ups_counts,ups_displacements,MPI_DOUBLE_PRECISION, &
       ups,(na+1)*nra,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_Scatterv(uoldps,ups_counts,ups_displacements,MPI_DOUBLE_PRECISION, &
       uoldps,(na+1)*nra,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_Scatterv(udotps,ups_counts,ups_displacements,MPI_DOUBLE_PRECISION, &
       udotps,(na+1)*nra,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_Scatterv(upoldp,ups_counts,ups_displacements,MPI_DOUBLE_PRECISION, &
       upoldp,(na+1)*nra,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  deallocate(dtm_counts,dtm_displacements,ups_counts,ups_displacements)

end subroutine mpisbv_comm

subroutine mpisbv(ndim,na,ncol,nint,ncb,nrc,nra,nca,ndx,iap,rap,par,icp, &
     rldot,ups,uoldps,udotps,upoldp,dtm,thu,ifst,nllv,comm_size)
  implicit none
  integer NIAP,NRAP,NPARX,NBIFX

  include 'mpif.h'
  include 'auto.h'

  integer, parameter :: NPARX2=2*NPARX
  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

  integer :: ndim,na,ncol,nint,ncb,nrc,nra,nca,ndx,iap(*),icp(*)
  integer :: ifst,nllv,comm_size
  double precision :: rap(*),par(*),rldot(*),dtm(*),thu(*)
  double precision :: ups(ndx,*),uoldps(ndx,*),udotps(ndx,*),upoldp(ndx,*)

  integer :: loop_start,loop_end,local_na,i,ierr,params(9)
  integer, allocatable :: buffer(:)
  integer pos,bufsize,size_int,size_double

  do i=2,comm_size
    
    ! Send message to get worker into setubv mode
    call MPI_Send(AUTO_MPI_SETUBV_MESSAGE,1,MPI_INTEGER,i-1,0, &
         MPI_COMM_WORLD,ierr)

    loop_start = ((i-1)*na)/comm_size
    loop_end = (i*na)/comm_size

    local_na = loop_end-loop_start
    call MPI_Send(local_na  ,1,MPI_INTEGER,i-1,0,MPI_COMM_WORLD,ierr)
  enddo

  params(1)=ndim
  params(2)=nra
  params(3)=nca
  params(4)=ncb
  params(5)=nrc
  params(6)=ncol
  params(7)=nint
  params(8)=ifst
  params(9)=nllv
  call MPI_Bcast(params,9,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  ! Here we compute the number of elements in the iap and rap structures.
  ! Since each of the structures is homogeneous we just divide the total
  ! size by the size of the individual elements.
  call MPI_Pack_size(NIAP+NPARX,MPI_INTEGER,MPI_COMM_WORLD,size_int,ierr)
  call MPI_Pack_size(NRAP+NPARX2+ndim*8+NPARX, &
                  MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,size_double,ierr)
  bufsize = size_int + size_double
  allocate(buffer(bufsize))

  pos = 0
  call MPI_Pack(iap,NIAP,MPI_INTEGER,buffer,bufsize,pos,MPI_COMM_WORLD,ierr)
  call MPI_Pack(rap,NRAP,MPI_DOUBLE_PRECISION,buffer,bufsize,pos, &
       MPI_COMM_WORLD,ierr)
  !**********************************************
  call MPI_Pack(par    ,NPARX2,MPI_DOUBLE_PRECISION,buffer,bufsize,pos, &
       MPI_COMM_WORLD,ierr)
  call MPI_Pack(icp    ,NPARX,MPI_INTEGER,buffer,bufsize,pos, &
       MPI_COMM_WORLD,ierr)

  call MPI_Pack(thu    ,ndim*8,MPI_DOUBLE_PRECISION,buffer,bufsize,pos, &
       MPI_COMM_WORLD,ierr)
  call MPI_Pack(rldot  ,NPARX,MPI_DOUBLE_PRECISION,buffer,bufsize,pos, &
       MPI_COMM_WORLD,ierr)

  call MPI_Bcast(buffer,pos,MPI_PACKED,0,MPI_COMM_WORLD,ierr)
  deallocate(buffer)

  call mpisbv_comm(na,nra,dtm,ups,uoldps,udotps,upoldp,comm_size)

  ! Worker runs here

end subroutine mpisbv

subroutine mpiinf(a,b,fa,sol,fc,na,nov,nra,nca,ncb,irf,icf,comm_size)
  implicit none
  include 'mpif.h'
  
  integer :: na,nov,nra,nca,ncb,irf(nra,*),icf(nca,*),comm_size
  double precision :: a(nca,nra,*),b(ncb,nra,*),fa(nra,*),fc(*),sol(nov,*)

  double precision, allocatable :: x(:)
  integer, allocatable :: sol_counts(:), sol_displacements(:)
  integer, allocatable :: fa_counts(:), fa_displacements(:)
  integer i, ierr, n, loop_end, loop_start

  allocate(sol_counts(comm_size),sol_displacements(comm_size))
  allocate(fa_counts(comm_size),fa_displacements(comm_size))

  do i=1,comm_size
    loop_start = ((i-1)*na)/comm_size
    loop_end = (i*na)/comm_size
    sol_counts(i) = nov*(loop_end-loop_start+1)
    sol_displacements(i) = nov*loop_start
    fa_counts(i) = nra*(loop_end-loop_start)
    fa_displacements(i) = nra*loop_start
  enddo
  if(comm_size>0)then
     sol_counts(1) = 0
     fa_counts(1) = 0
  endif

  call MPI_Bcast(fc,nov+ncb,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_Scatterv(sol,sol_counts,sol_displacements,MPI_DOUBLE_PRECISION, &
       sol,nov*(na+1),MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  allocate(x(nra))
  n = na
  if(comm_size/=0)then
     n = na/comm_size
  endif
  call infpar(a,b,fa,sol,fc,n,nov,nra,nca,ncb,irf,icf,x)
  deallocate(x)

  call MPI_Gatherv(fa,nra*na,MPI_DOUBLE_PRECISION, &
       fa,fa_counts,fa_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  deallocate(fa_counts,fa_displacements,sol_counts,sol_displacements)

end subroutine mpiinf

subroutine mpiend()
  implicit none
  include 'mpif.h'

  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

  integer size,i,ierr

  call MPI_Comm_size(MPI_COMM_WORLD,size,ierr)
  do i=1,size-1
     call MPI_Send(AUTO_MPI_KILL_MESSAGE,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,ierr)
  enddo

  call MPI_Finalize(ierr)
end subroutine mpiend
