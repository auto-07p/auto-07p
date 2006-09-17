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
  integer, parameter :: AUTO_MPI_CONPAR_MESSAGE = 2, AUTO_MPI_INIT_MESSAGE = 3

  integer ierr,myid,namelen
  character(len=MPI_MAX_PROCESSOR_NAME) processor_name

  call MPI_Init(ierr)
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
  integer, parameter :: AUTO_MPI_CONPAR_MESSAGE = 2, AUTO_MPI_INIT_MESSAGE = 3

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
  integer, parameter :: AUTO_MPI_CONPAR_MESSAGE = 2, AUTO_MPI_INIT_MESSAGE = 3

  logical :: autobv
  external funi,icni

  double precision, allocatable :: aa(:,:,:), bb(:,:,:), cc(:,:,:), dd(:,:)
  double precision, allocatable :: fa(:,:), fc(:)

  integer :: message_type, ierr, stat(MPI_STATUS_SIZE), na
  integer :: params(8)

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
        call MPI_Bcast(params,8,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
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

    integer :: nov, nra, nca, ncb, nrc, nint
    integer, allocatable :: irf(:,:), icf(:,:)
    double precision dum

    ! input scalars
    nov = params(1)
    nra = params(2)
    nca = params(3)
    ncb = params(4)
    nrc = params(5)
    nint = nrc-1

    !input/output arrays

    allocate(irf(nra,na),icf(nca,na))

    call conpar(nov,na,nra,nca,aa,ncb,bb,nrc,cc,dd,irf,icf)

    call mpicon_comm(na, nra, nca, aa, ncb, bb, nrc, cc, dd, dum, fa, fc, &
         dum, irf, icf, 0)

    deallocate(irf,icf)
    deallocate(aa,bb,cc,dd,fa)
    if(nint>0)deallocate(fc)
  end subroutine mpi_conpar_worker

  subroutine mpi_setubv_worker(funi,icni,na,params)
    implicit none
    include 'mpif.h'
    integer NIAP,NRAP,NPARX,NBIFX
    include 'auto.h'
    integer, parameter :: NPARX2=2*NPARX

    integer :: na, params(8)

    integer :: ndim, ncol, nint, ncb, nrc, nra, nca
    integer :: i, j, ierr

    integer, allocatable :: iap(:),icp(:)
    double precision, allocatable :: rap(:), par(:), ups(:,:), uoldps(:,:)
    double precision, allocatable :: wp(:,:), wt(:,:), wi(:), udotps(:,:)
    double precision, allocatable :: upoldp(:,:), thu(:), rldot(:)
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

    allocate(iap(NIAP),rap(NRAP),par(NPARX2),icp(NPARX))
    allocate(wp(ncol+1,ncol),wt(ncol+1,ncol),wi(ncol+1))
    allocate(thu(ndim*8),rldot(NPARX))

    ! Here we compute the number of elements in the iap and rap structures.
    ! Since each of the structures is homogeneous we just divide the total
    ! size by the size of the individual elements.

    call MPI_Pack_size(NIAP+NPARX,MPI_INTEGER,MPI_COMM_WORLD,size_int,ierr)
    call MPI_Pack_size(NRAP+NPARX2+ &
         (ncol + 1)*ncol+    &
         (ncol + 1)*ncol+    &
         (ncol + 1)+         &
         ndim*8+             &
         NPARX,              &
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
    call MPI_Unpack(buffer,bufsize,pos,wp    ,(ncol + 1)*ncol, &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
    call MPI_Unpack(buffer,bufsize,pos,wt    ,(ncol + 1)*ncol, &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
    call MPI_Unpack(buffer,bufsize,pos,wi    ,(ncol + 1), &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
    call MPI_Unpack(buffer,bufsize,pos,thu   ,ndim*8, &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
    call MPI_Unpack(buffer,bufsize,pos,rldot ,NPARX, &
         MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
    deallocate(buffer)

    allocate(dtm(na))
    allocate(ups(nra,na+1),uoldps(nra,na+1),udotps(nra,na+1),upoldp(nra,na+1))
    call mpisbv_comm(na,nra,dtm,ups,uoldps,udotps,upoldp,0)

    ! output arrays

    allocate(aa(nca,nra,na),bb(ncb,nra,na),cc(nca,nrc,na),dd(ncb,nrc))
    allocate(fa(nra,na))
    if(nint>0) allocate(fc(nint))

    ! A little explanation of what is going on here
    ! is in order I believe.  This array is
    ! created by a summation across all workers,
    ! hence it needs a summation in the master.
    !
    ! We sum into dd, which is a local variable initialized to
    ! 0.0. We then sum our part with the masters part
    ! in the master.
    !
    ! zero pseudo-arclength part of array, rest is done in setubv()

    do i=nint+1,nrc
       do j=1,ncb
          dd(j,i)=0.0d0
       enddo
    enddo

    call subvpi(ndim, na, ncol, nint, ncb, nrc, nra, nca, funi, icni, nra, &
         iap, rap, par, icp, aa, bb, cc, dd, fa, fc, ups, &
         uoldps, udotps, upoldp, dtm, thu, wi, wp, wt)

    ! free input arrays
    deallocate(iap,rap,par,icp,ups,uoldps,dtm,wp,wt,udotps,upoldp,wi,thu)
    deallocate(rldot)
  end subroutine mpi_setubv_worker

end subroutine mpiwfi

subroutine mpicon_comm(na, nra, nca, a, ncb, b, nrc, c, d, drec, fa, fc, &
     fcrec, irf, icf, comm_size)
  implicit none
  include 'mpif.h'

  integer :: na, nra, nca, ncb, nrc, nint
  integer :: icf(nca,*), irf(nra,*), comm_size
  double precision :: a(nca,nra,*),b(ncb,nra,*),c(nca,nrc,*)
  double precision :: d(ncb,*),drec(ncb,*),fa(nra,*),fc(*),fcrec(*)

  integer :: i,ierr,loop_start,loop_end
  integer, allocatable :: a_counts(:),a_displacements(:)
  integer, allocatable :: b_counts(:),b_displacements(:)
  integer, allocatable :: c_counts(:),c_displacements(:)
  integer, allocatable :: irf_counts(:),irf_displacements(:)
  integer, allocatable :: icf_counts(:),icf_displacements(:)
  integer, allocatable :: fa_counts(:), fa_displacements(:)

  allocate(a_counts(comm_size),a_displacements(comm_size))
  allocate(b_counts(comm_size),b_displacements(comm_size))
  allocate(c_counts(comm_size),c_displacements(comm_size))
  allocate(irf_counts(comm_size),irf_displacements(comm_size))
  allocate(icf_counts(comm_size),icf_displacements(comm_size))
  allocate(fa_counts(comm_size),fa_displacements(comm_size))

  do i=1,comm_size
     loop_start = ((i-1)*na)/comm_size
     loop_end = (i*na)/comm_size
     if(i==1)loop_end = 0
     a_counts(i) = nca*nra*(loop_end-loop_start)
     a_displacements(i) = nca*nra*loop_start
     b_counts(i) = ncb*nra*(loop_end-loop_start)
     b_displacements(i) = ncb*nra*loop_start
     c_counts(i) = nca*nrc*(loop_end-loop_start)
     c_displacements(i) = nca*nrc*loop_start
     irf_counts(i) = nra*(loop_end-loop_start)
     irf_displacements(i) = nra*loop_start
     icf_counts(i) = nca*(loop_end-loop_start)
     icf_displacements(i) = nca*loop_start
     fa_counts(i) = nra*(loop_end-loop_start)
     fa_displacements(i) = nra*loop_start
  enddo

  call MPI_Gatherv(a, nca*nra*na,MPI_DOUBLE_PRECISION, &
       a,a_counts,a_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  call MPI_Gatherv(b, ncb*nra*na,MPI_DOUBLE_PRECISION, &
       b,b_counts,b_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  call MPI_Gatherv(c, nca*nrc*na,MPI_DOUBLE_PRECISION, &
       c,c_counts,c_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  call MPI_Gatherv(irf,na*nra,MPI_INTEGER, &
       irf,irf_counts,irf_displacements,MPI_INTEGER, &
       0,MPI_COMM_WORLD,ierr)
  call MPI_Gatherv(icf,na*nca,MPI_INTEGER, &
       icf,icf_counts,icf_displacements,MPI_INTEGER, &
       0,MPI_COMM_WORLD,ierr)

  call MPI_Gatherv(fa,nra*na,MPI_DOUBLE_PRECISION, &
       fa,fa_counts,fa_displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)

  deallocate(a_counts,a_displacements,b_counts,b_displacements)
  deallocate(c_counts,c_displacements,irf_counts,irf_displacements)
  deallocate(icf_counts,icf_displacements,fa_counts,fa_displacements)

  call MPI_Reduce(d,drec,ncb*nrc,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  nint=nrc-1
  if(nint>0)then
     call MPI_Reduce(fc,fcrec,nint,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
          MPI_COMM_WORLD,ierr)
  endif

end subroutine mpicon_comm

subroutine mpicon(na, nra, nca, a, ncb, b, nrc, c, d, fa, fc, irf, icf, &
     comm_size)
  implicit none
  include 'mpif.h'

  integer :: na, nra, nca, ncb, nrc, nint
  integer :: icf(nca,*), irf(nra,*), comm_size
  double precision :: a(nca,nra,*),b(ncb,nra,*),c(nca,nrc,*)
  double precision :: d(ncb,*),fa(nra,*),fc(*)

  integer i,j
  double precision, allocatable :: dtemp(:,:),fctemp(:)

  ! Worker is running now

  ! I create a temporary receive buffer for the MPI_Reduce
  ! command.  This is because there isn't an
  ! asymmetric version (like MPI_Scatterv).
  allocate(dtemp(ncb,nrc))
  nint=nrc-1
  if(nint>0)allocate(fctemp(nint))

  call mpicon_comm(na, nra, nca, a, ncb, b, nrc, c, d, dtemp, fa, fc, &
     fctemp, irf, icf, comm_size)

  do i=1,nrc
     do j=1,ncb
        d(i,j)=dtemp(j,i)
     enddo
  enddo
  deallocate(dtemp)
  if(nint>0)then
     do i=1,nint
        fc(i)=fctemp(i)
     enddo
     deallocate(fctemp)
  endif

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
  dtm_counts(1) = 0
  ups_counts(1) = 0

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
     rldot,ups,uoldps,udotps,upoldp,dtm,thu,wi,wp,wt,comm_size)
  implicit none
  integer NIAP,NRAP,NPARX,NBIFX

  include 'mpif.h'
  include 'auto.h'

  integer, parameter :: NPARX2=2*NPARX
  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_CONPAR_MESSAGE = 2, AUTO_MPI_INIT_MESSAGE = 3

  integer :: ndim,na,ncol,nint,ncb,nrc,nra,nca,ndx,iap(*),icp(*),comm_size
  double precision :: rap(*),par(*),rldot(*)
  double precision :: ups(ndx,*),uoldps(ndx,*),udotps(ndx,*),upoldp(ndx,*)
  double precision :: dtm(*),thu(*),wi(*),wp(ncol+1,*),wt(ncol+1,*)

  integer :: loop_start,loop_end,local_na,i,ierr,params(7)
  integer, allocatable :: buffer(:)
  integer pos,bufsize,size_int,size_double

  call MPI_Comm_size(MPI_COMM_WORLD,comm_size,ierr)
  if(comm_size<2)return
  
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
  call MPI_Bcast(params,7,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  ! Here we compute the number of elements in the iap and rap structures.
  ! Since each of the structures is homogeneous we just divide the total
  ! size by the size of the individual elements.
  call MPI_Pack_size(NIAP+NPARX,MPI_INTEGER,MPI_COMM_WORLD,size_int,ierr)
  call MPI_Pack_size(NRAP+NPARX2+ &
                  (ncol + 1)*ncol+  &
                  (ncol + 1)*ncol+  &
                  (ncol + 1)+       &
                  ndim*8+           &
                  NPARX,            &
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
  call MPI_Pack(wp     ,(ncol + 1)*ncol,MPI_DOUBLE_PRECISION,buffer,bufsize, &
       pos,MPI_COMM_WORLD,ierr)
  call MPI_Pack(wt     ,(ncol + 1)*ncol,MPI_DOUBLE_PRECISION,buffer,bufsize, &
       pos,MPI_COMM_WORLD,ierr)
  call MPI_Pack(wi     ,ncol + 1,MPI_DOUBLE_PRECISION,buffer,bufsize,pos, &
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

subroutine mpiend()
  implicit none
  include 'mpif.h'

  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_CONPAR_MESSAGE = 2, AUTO_MPI_INIT_MESSAGE = 3

  integer size,i,ierr

  call MPI_Comm_size(MPI_COMM_WORLD,size,ierr)
  do i=1,size-1
     call MPI_Send(AUTO_MPI_KILL_MESSAGE,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,ierr)
  enddo

  call MPI_Finalize(ierr)
end subroutine mpiend
