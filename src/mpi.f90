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
! mpisbv: sends solvbv input from master to workers
! mpicon: sends a1, a2 etc from workers to master after conpar.
! mpibcast: front-end to MPI_Bcast
! mpigat: front-end to MPI_Gatherv
! mpiscat: front-end to MPI_Scatterv
! mpisum: front-end to MPI_Reduce (summation)
! mpiend: master tells workers to stop

subroutine mpiini(iap)
  implicit none
  include 'mpif.h'

  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

  integer ierr,iam,namelen,iap(*)
  character(len=MPI_MAX_PROCESSOR_NAME) processor_name

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,iap(39),ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  iap(38) = iam
  call MPI_Get_processor_name(processor_name,namelen,ierr)
!  print *,'Process ',iam,' on ',processor_name
  if(iam/=0)then
     call mpi_worker()
  endif

contains

  subroutine mpi_worker()
    use auto_constants
    implicit none
    include 'mpif.h'

    integer :: message_type, ierr
    integer :: funi_icni_params(5), iap(NIAP)
    double precision :: rap,par

    call MPI_Bcast(message_type,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
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
       ips     = funi_icni_params(1)
       iap(2)  = ips
       irs     = funi_icni_params(2)
       iap(3)  = irs
       isw     = funi_icni_params(3)
       iap(10) = isw
       iap(27) = funi_icni_params(4) ! itp
       iap(29) = funi_icni_params(5) ! nfpr
       iap(38) = 1                   ! iam
       call autoi(iap,rap,par)
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
  integer ierr
  integer funi_icni_params(5)

  funi_icni_params(1)=iap(2)  ! ips
  funi_icni_params(2)=iap(3)  ! irs
  funi_icni_params(3)=iap(10) ! isw
  funi_icni_params(4)=iap(27) ! itp
  funi_icni_params(5)=iap(29) ! nfpr
  ! Send message to get worker into init mode
  call MPI_Bcast(AUTO_MPI_INIT_MESSAGE,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_Bcast(funi_icni_params,5,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

end subroutine mpiiap

subroutine mpiwfi(autobv,funi,icni)
  implicit none
  include 'mpif.h'

  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

  logical :: autobv
  external funi,icni

  integer :: message_type, ierr

  if (.not.autobv) then
     print *,'Illegal problem type for MPI'
     call MPI_Finalize(ierr)
     stop
  endif

  do while(.true.)
     call MPI_Bcast(message_type,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  
     select case(message_type)
     case(AUTO_MPI_KILL_MESSAGE) ! The kill message
        call MPI_Finalize(ierr)
        stop
     case(AUTO_MPI_INIT_MESSAGE)
        return
     case(AUTO_MPI_SETUBV_MESSAGE) ! The setubv message
        call mpi_setubv_worker(funi,icni)
     case default
        print *,'Unknown message recieved: ', message_type
     end select
  enddo

contains

  subroutine mpi_setubv_worker(funi,icni)
    use solvebv
    use interfaces, only:bcni
    implicit none
    include 'mpif.h'
    integer NIAP,NRAP,NPARX,NBIFX
    include 'auto.h'
    integer, parameter :: NPARX2=2*NPARX

    integer :: ndim, nra, nfc, ifst, nllv, na, iam, kwt, nbc, ncol, nint, ntst
    integer :: ierr

    integer :: iap(NIAP),icp(NPARX)
    double precision :: rap(NRAP),par(NPARX2),rldot(NPARX)
    double precision, allocatable :: ups(:,:), uoldps(:,:)
    double precision, allocatable :: udotps(:,:), upoldp(:,:), thu(:)
    double precision, allocatable :: dtm(:),fa(:,:), fc(:)
    integer, allocatable :: np(:)
    double precision :: dum,dum1(1)

    external funi, icni

    call MPI_Bcast(iap,NIAP,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,iap(39),ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD,iap(38),ierr)

    ndim=iap(1)
    ntst=iap(5)
    ncol=iap(6)
    nbc=iap(12)
    nint=iap(13)
    iam=iap(38)
    kwt=iap(39)

    allocate(np(kwt))
    call partition(ntst,kwt,np)
    na=np(iam+1)
    deallocate(np)
    nra=ndim*ncol
    nfc=nbc+nint+1

    allocate(thu(ndim*8),dtm(na))
    allocate(ups(nra,na+1),uoldps(nra,na+1),udotps(nra,na+1),upoldp(nra,na+1))
    ! output arrays
    allocate(fa(nra,na),fc(nfc))

    call mpisbv(iap,rap,par,icp,rldot,nra,ups,uoldps,udotps,upoldp, &
         dtm,thu,ifst,nllv)
    call solvbv(ifst,iap,rap,par,icp,funi,bcni,icni,dum, &
         nllv,dum1,dum1,rldot,nra,ups,dum1,uoldps,udotps,upoldp,dtm, &
         fa,fc,dum1,dum1,dum1,thu)

    ! free input arrays
    deallocate(ups,uoldps,dtm,udotps,upoldp,thu)

    deallocate(fa,fc)

  end subroutine mpi_setubv_worker

end subroutine mpiwfi

subroutine mpicon(s1,a1,a2,bb,cc,d,faa,fc,ntst,nov,ncb,nrc,ifst)
  use solvebv, only: partition
  implicit none
  include 'mpif.h'

  integer :: ntst, nov, ncb, nrc, ifst
  double precision :: a1(nov,nov,*),a2(nov,nov,*),bb(ncb,nov,*),cc(nov,nrc,*)
  double precision :: s1(nov,nov,*),d(ncb,*),faa(nov,*),fc(*)

  double precision, allocatable :: ccb(:,:,:)
  integer,allocatable :: np(:)
  integer :: i,ii,j,k,ierr,iam,kwt

  ! Worker is running now

  call mpigat(faa,nov,ntst)
  call mpisum(fc,nrc)

  if(ifst==0)return

  call mpigat(s1,nov*nov,ntst)
  call mpigat(a1,nov*nov,ntst)
  call mpigat(a2,nov*nov,ntst)
  call mpigat(bb,nov*ncb,ntst)

  call MPI_Comm_size(MPI_COMM_WORLD,kwt,ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  allocate(ccb(nov,nrc,kwt),np(kwt))
  call partition(ntst,kwt,np)
  call MPI_Gather(cc(1,1,np(iam+1)+1),nov*nrc,MPI_DOUBLE_PRECISION, &
       ccb,nov*nrc,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  call mpigat(cc,nov*nrc,ntst)

  if(iam==0)then
     ii = 0
     do i=1,kwt
        !fix up boundaries between cc parts
        ii = ii+np(i)
        do j=1,nrc
           do k=1,nov
              if(i==kwt)then
                 cc(k,j,ii+1)=ccb(k,j,i)
              else
                 cc(k,j,ii+1)=cc(k,j,ii+1)+ccb(k,j,i)
              endif
           enddo
        enddo
     enddo
  endif
  deallocate(ccb,np)

  call mpisum(d,ncb*nrc)
end subroutine mpicon

subroutine mpisbv(iap,rap,par,icp,rldot,nra,ups,uoldps,udotps,upoldp,dtm, &
     thu,ifst,nllv)
  implicit none
  integer NIAP,NRAP,NPARX,NBIFX

  include 'mpif.h'
  include 'auto.h'

  integer, parameter :: NPARX2=2*NPARX
  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

  integer :: nra,iap(*),icp(*),ifst,nllv
  double precision :: rap(*),par(*),rldot(*),dtm(*),thu(*)
  double precision :: ups(nra,*),uoldps(nra,*),udotps(nra,*),upoldp(nra,*)

  external funi, icni

  integer :: ierr,ntst,ndim,iam,kwt
  integer :: pos,bufsize,size_int,size_double
  character*1, allocatable :: buffer(:)

  iam=iap(38)
  kwt=iap(39)
  if(iam==0)then
     ! Send message to get worker into setubv mode
     call MPI_Bcast(AUTO_MPI_SETUBV_MESSAGE,1,MPI_INTEGER,0, &
             MPI_COMM_WORLD,ierr)
     call MPI_Bcast(iap,NIAP,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  endif

  ndim=iap(1)
  call MPI_Pack_size(2+NPARX,MPI_INTEGER,MPI_COMM_WORLD,size_int,ierr)
  call MPI_Pack_size(NRAP+NPARX2+ndim*8+NPARX, &
                  MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,size_double,ierr)
  bufsize = size_int + size_double
  allocate(buffer(bufsize))

  pos = 0

  if(iam==0)then
     call MPI_Pack(ifst,1,MPI_INTEGER,buffer,bufsize,pos,MPI_COMM_WORLD,ierr)
     call MPI_Pack(nllv,1,MPI_INTEGER,buffer,bufsize,pos,MPI_COMM_WORLD,ierr)
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
  endif

  call MPI_Bcast(buffer,bufsize,MPI_PACKED,0,MPI_COMM_WORLD,ierr)

  if(iam>0)then
     call MPI_Unpack(buffer,bufsize,pos,ifst  ,1, &
          MPI_INTEGER,MPI_COMM_WORLD,ierr)
     call MPI_Unpack(buffer,bufsize,pos,nllv  ,1, &
          MPI_INTEGER,MPI_COMM_WORLD,ierr)
     call MPI_Unpack(buffer,bufsize,pos,rap   ,NRAP, &
          MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
     ! /***********************************/
     call MPI_Unpack(buffer,bufsize,pos,par   ,NPARX2, &
          MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
     call MPI_Unpack(buffer,bufsize,pos,icp   ,NPARX, &
          MPI_INTEGER,MPI_COMM_WORLD,ierr)
     ndim=iap(1)
     call MPI_Unpack(buffer,bufsize,pos,thu   ,ndim*8, &
          MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
     call MPI_Unpack(buffer,bufsize,pos,rldot ,NPARX, &
          MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
  endif

  deallocate(buffer)

  ntst=iap(5)
  call mpiscat(dtm,1,ntst,0)
  call mpiscat(ups,nra,ntst,1)
  call mpiscat(uoldps,nra,ntst,1)
  call mpiscat(udotps,nra,ntst,1)
  call mpiscat(upoldp,nra,ntst,1)

  ! Worker runs here

end subroutine mpisbv

subroutine mpibcast(buf,len)
  implicit none
  include 'mpif.h'

  integer :: len
  double precision :: buf(len)

  integer :: ierr

  call MPI_Bcast(buf,len,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
end subroutine mpibcast

subroutine mpiscat(buf,ndx,n,add)
  use solvebv, only: partition
  implicit none
  include 'mpif.h'

  integer ndx,n,add
  double precision :: buf(ndx,*)

  integer, allocatable :: counts(:), displacements(:), np(:)
  integer i, ierr, loop_start, iam, kwt, na0

  call MPI_Comm_size(MPI_COMM_WORLD,kwt,ierr)
  allocate(np(kwt))
  call partition(n,kwt,np)

  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  if(iam==0)then
     allocate(counts(kwt),displacements(kwt))
     loop_start = 0
     do i=1,kwt
        counts(i) = ndx*(np(i)+add)
        displacements(i) = ndx*loop_start
        loop_start = loop_start + np(i)
     enddo
     counts(1) = 0
  endif

  na0=(np(iam+1)+add)*ndx
  if(iam==0)na0=0
  call MPI_Scatterv(buf,counts,displacements,MPI_DOUBLE_PRECISION, &
       buf,na0,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  deallocate(np)
  if(iam==0)deallocate(counts,displacements)
end subroutine mpiscat

subroutine mpigat(buf,ndx,n)
  use solvebv, only: partition
  implicit none
  include 'mpif.h'

  integer ndx,n
  double precision :: buf(ndx,*)

  integer, allocatable :: counts(:), displacements(:), np(:)
  integer i, ierr, loop_start, iam, kwt, na0

  call MPI_Comm_size(MPI_COMM_WORLD,kwt,ierr)
  allocate(np(kwt))
  call partition(n,kwt,np)

  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  if(iam==0)then
     allocate(counts(kwt),displacements(kwt))
     loop_start = 0
     do i=1,kwt
        counts(i) = ndx*np(i)
        displacements(i) = ndx*loop_start
        loop_start = loop_start + np(i)
     enddo
     counts(1) = 0
  endif

  na0=np(iam+1)*ndx
  if(iam==0)na0=0
  call MPI_Gatherv(buf,na0,MPI_DOUBLE_PRECISION, &
       buf,counts,displacements,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)

  deallocate(np)
  if(iam==0)deallocate(counts,displacements)
end subroutine mpigat

subroutine mpisum(buf,len)
  implicit none
  include 'mpif.h'

  integer :: len
  double precision :: buf(len)

  double precision, allocatable :: temp(:)
  integer ierr, i, iam

  ! A little explanation of what is going on here
  ! is in order I believe.  This array is
  ! created by a summation across all workers,
  ! hence it needs a summation in the master.
  !
  ! We sum into d, which is a local variable initialized to
  ! 0.0. We then sum our part with the masters part
  ! in the master.
  !
  ! I create a temporary receive buffer for the MPI_Reduce
  ! command.  This is because there isn't an
  ! asymmetric version (like MPI_Scatterv).
  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  if(iam==0)allocate(temp(len))
  call MPI_Reduce(buf,temp,len,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)
  if(iam==0)then
     do i=1,len
        buf(i)=temp(i)
     enddo
     deallocate(temp)
  endif
end subroutine mpisum

subroutine mpiend()
  implicit none
  include 'mpif.h'

  integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
  integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

  integer ierr

  call MPI_Bcast(AUTO_MPI_KILL_MESSAGE,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  call MPI_Finalize(ierr)
end subroutine mpiend

subroutine mpitim(tim)
  implicit none
  include 'mpif.h'

  double precision tim
  tim = MPI_Wtime()
end subroutine mpitim  
