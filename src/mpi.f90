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
! mpireduce: MPI communication for REDUCE
! mpibcksub: MPI communication for BCKSUB
! mpibcast: front-end to MPI_Bcast
! mpigat: front-end to MPI_Gatherv
! mpiend: master tells workers to stop

module autompi

use auto_constants, only: autoparameters, niap, nrap

implicit none
private

public :: mpiini, mpiiap, mpiwfi, mpireduce, mpibcksub, mpisbv, mpibcast
public :: mpibcasti, mpibcastap
public :: mpigat, mpiend, mpitim, mpiiam, mpikwt, partition

integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SETUBV_MESSAGE = 1
integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

include 'mpif.h'

contains

subroutine mpiini()

  integer ierr,iam,namelen
  character(len=MPI_MAX_PROCESSOR_NAME) processor_name
  integer :: message_type

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  call MPI_Get_processor_name(processor_name,namelen,ierr)
!  print *,'Process ',iam,' on ',processor_name
  if(iam/=0)then
    call MPI_Bcast(message_type,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    if(message_type /= AUTO_MPI_INIT_MESSAGE)then
       print *,'Fatal: no init message, message received: ', message_type
       call MPI_Finalize(ierr)
       stop
    endif
 endif

end subroutine mpiini

integer function mpiiam()
  integer ierr
  call MPI_Comm_rank(MPI_COMM_WORLD,mpiiam,ierr)
end function mpiiam

integer function mpikwt()
  integer ierr
  call MPI_Comm_size(MPI_COMM_WORLD,mpikwt,ierr)
end function mpikwt

subroutine mpiiap(ap)
  type(autoparameters) :: ap

  integer ierr

  ! Send message to get worker into init mode
  call MPI_Bcast(AUTO_MPI_INIT_MESSAGE,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call mpibcastap(ap)

end subroutine mpiiap

logical function mpiwfi(autobv)
  logical :: autobv

  integer :: message_type, ierr

  mpiwfi = .false.
  if (.not.autobv) then
     print *,'Illegal problem type for MPI'
     call MPI_Finalize(ierr)
     stop
  endif

  call MPI_Bcast(message_type,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  
  select case(message_type)
  case(AUTO_MPI_KILL_MESSAGE) ! The kill message
     call MPI_Finalize(ierr)
     stop
  case(AUTO_MPI_INIT_MESSAGE)
     ! do nothing
  case(AUTO_MPI_SETUBV_MESSAGE) ! The setubv message
     mpiwfi = .true.
  case default
     print *,'Unknown message recieved: ', message_type
  end select
end function mpiwfi

!--------- ---------
subroutine partition(n,kwt,m)
!
! Linear distribution of NTST over all nodes
  integer n,kwt,m(kwt)
  integer i
  do i=1,kwt
     m(i) = (i*n+kwt-1)/kwt - ((i-1)*n+kwt-1)/kwt
  enddo
end subroutine partition

subroutine mpireduce(a1,a2,bb,cc,c2,dd,faa,fcfc,ntst,nov,ncb,nrc,ifst,nllv,&
     lo,hi)
  integer, intent(in) :: ntst,nov,ncb,nrc,ifst,nllv,lo,hi
  double precision, intent(inout) :: a1(nov,nov,*),a2(nov,nov,*),bb(ncb,nov,*)
  double precision, intent(inout) :: cc(nov,nrc,*),c2(nov,nrc,*)
  double precision, intent(inout) :: dd(ncb,nrc,*),faa(nov,*)
  double precision, intent(inout) :: fcfc(nrc,*)

  integer :: iam,kwt,mid,nlo,nmid1

  iam=mpiiam()
  kwt=mpikwt()
  mid=(lo+hi)/2
  nlo=(lo-1)*kwt/ntst
  nmid1=mid*kwt/ntst
  if(nmid1==iam.and.nlo<iam)then
     if(nllv.eq.0)then
        call mpisend(faa(1,hi),nov,nlo)
        call mpisend(fcfc(1,hi),nrc,nlo)
     endif
     if(ifst.eq.1)then
        ! send hi to lo
        call mpisend(a1(1,1,mid+1),nov*nov,nlo)
        call mpisend(a2(1,1,hi),nov*nov,nlo)
        call mpisend(bb(1,1,hi),nov*ncb,nlo)
        call mpisend(cc(1,1,mid+1),nov*nrc,nlo)
        call mpisend(c2(1,1,hi),nov*nrc,nlo)
        call mpisend(dd(1,1,hi),ncb*nrc,nlo)
     endif
  elseif(nmid1>iam.and.nlo==iam)then
     if(nllv.eq.0)then
        call mpirecv(faa(1,hi),nov,nmid1)
        call mpirecv(fcfc(1,hi),nrc,nmid1)
     else
        faa(:,hi)=0
        fcfc(:,hi)=0
     endif
     if(ifst.eq.1)then
        ! receive hi
        call mpirecv(a1(1,1,mid+1),nov*nov,nmid1)
        call mpirecv(a2(1,1,hi),nov*nov,nmid1)
        call mpirecv(bb(1,1,hi),nov*ncb,nmid1)
        call mpirecv(cc(1,1,mid+1),nov*nrc,nmid1)
        call mpirecv(c2(1,1,hi),nov*nrc,nmid1)
        call mpirecv(dd(1,1,hi),ncb*nrc,nmid1)
     endif
  endif
end subroutine mpireduce

subroutine mpibcksub(sol,ntst,nov,lo,hi)
  integer, intent(in) :: ntst,nov,lo,hi
  double precision, intent(inout) :: sol(nov,*)
  integer :: iam,kwt,mid,nlo,nmid1

  iam=mpiiam()
  kwt=mpikwt()
  mid=(lo+hi)/2
  nlo=(lo-1)*kwt/ntst
  nmid1=mid*kwt/ntst
  if(nmid1==iam.and.nlo<iam)then
     call mpirecv(sol(1,mid+1),nov,nlo)
     call mpirecv(sol(1,hi+1),nov,nlo)
  elseif(nmid1>iam.and.nlo==iam)then
     call mpisend(sol(1,mid+1),nov,nmid1)
     call mpisend(sol(1,hi+1),nov,nmid1)
  endif
end subroutine mpibcksub

subroutine mpirecv(a,isize,isrc)
  integer, intent(in) :: isize, isrc
  double precision, intent(out) :: a(isize)

  integer ierr
  integer status(MPI_STATUS_SIZE)
  call MPI_Recv(a,isize,MPI_DOUBLE_PRECISION,isrc, &
       MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
end subroutine mpirecv

subroutine mpisend(a,isize,idest)
  integer, intent(in) :: isize, idest
  double precision, intent(in) :: a(isize)

  integer ierr
  call MPI_Send(a,isize,MPI_DOUBLE_PRECISION,idest, &
       0, MPI_COMM_WORLD, ierr)
end subroutine mpisend

subroutine mpisbv(ap,par,icp,ndim,ups,uoldps,rlold,udotps,upoldp,dtm, &
     thu,ifst,nllv)

  type(autoparameters) :: ap
  integer, intent(in) :: ndim,icp(*)
  integer, intent(inout) :: ifst,nllv
  double precision :: par(*),dtm(*),thu(*)
  double precision :: ups(ndim,0:*),uoldps(ndim,0:*),udotps(ndim,0:*),upoldp(ndim,0:*)
  double precision :: rlold(ap%nfpr)

  integer :: ncol,npar,ierr,ntst,iam,nint,nfpr
  integer :: pos,bufsize,size_int,size_double
  character*1, allocatable :: buffer(:)

  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  if(iam==0)then
     ! Send message to get worker into setubv mode
     call MPI_Bcast(AUTO_MPI_SETUBV_MESSAGE,1,MPI_INTEGER,0, &
             MPI_COMM_WORLD,ierr)
     call mpibcastap(ap)
  endif

  nint=ap%nint
  nfpr=ap%nfpr
  npar=ap%npar
  call MPI_Pack_size(2+nfpr+nint,MPI_INTEGER,MPI_COMM_WORLD,size_int,ierr)
  call MPI_Pack_size(npar+nfpr+ndim, &
                  MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,size_double,ierr)
  bufsize = size_int + size_double
  allocate(buffer(bufsize))

  pos = 0

  if(iam==0)then
     call MPI_Pack(ifst,1,MPI_INTEGER,buffer,bufsize,pos,MPI_COMM_WORLD,ierr)
     call MPI_Pack(nllv,1,MPI_INTEGER,buffer,bufsize,pos,MPI_COMM_WORLD,ierr)
     !**********************************************
     call MPI_Pack(par    ,npar,MPI_DOUBLE_PRECISION,buffer,bufsize,pos, &
          MPI_COMM_WORLD,ierr)
     call MPI_Pack(rlold  ,nfpr,MPI_DOUBLE_PRECISION,buffer,bufsize,pos, &
          MPI_COMM_WORLD,ierr)
     call MPI_Pack(icp    ,nfpr+nint,MPI_INTEGER,buffer,bufsize,pos, &
          MPI_COMM_WORLD,ierr)

     call MPI_Pack(thu    ,ndim,MPI_DOUBLE_PRECISION,buffer,bufsize,pos, &
          MPI_COMM_WORLD,ierr)
  endif

  call MPI_Bcast(buffer,bufsize,MPI_PACKED,0,MPI_COMM_WORLD,ierr)

  if(iam>0)then
     call MPI_Unpack(buffer,bufsize,pos,ifst  ,1, &
          MPI_INTEGER,MPI_COMM_WORLD,ierr)
     call MPI_Unpack(buffer,bufsize,pos,nllv  ,1, &
          MPI_INTEGER,MPI_COMM_WORLD,ierr)
     ! /***********************************/
     call MPI_Unpack(buffer,bufsize,pos,par   ,npar, &
          MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
     call MPI_Unpack(buffer,bufsize,pos,rlold ,nfpr, &
          MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
     call MPI_Unpack(buffer,bufsize,pos,icp   ,nfpr+nint, &
          MPI_INTEGER,MPI_COMM_WORLD,ierr)
     call MPI_Unpack(buffer,bufsize,pos,thu   ,ndim, &
          MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,ierr)
  endif

  deallocate(buffer)

  ntst=ap%ntst
  ncol=ap%ncol
  call mpiscat(dtm,1,ntst,0)
  call mpiscat(ups,ndim*ncol,ntst,ndim)
  call mpiscat(uoldps,ndim*ncol,ntst,ndim)
  call mpiscat(udotps,ndim*ncol,ntst,ndim)
  call mpiscat(upoldp,ndim*ncol,ntst,ndim)

  ! Worker runs here

end subroutine mpisbv

subroutine mpibcast(buf,len)
  integer, intent(in) :: len
  double precision, intent(inout) :: buf(len)

  integer :: ierr

  call MPI_Bcast(buf,len,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
end subroutine mpibcast

subroutine mpibcasti(buf,len)
  integer, intent(in) :: len
  integer, intent(inout) :: buf(len)

  integer :: ierr

  call MPI_Bcast(buf,len,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
end subroutine mpibcasti

subroutine mpibcastap(ap)
  type(autoparameters), intent(inout) :: ap

  integer :: ierr

  call MPI_Bcast(ap%ndim,NIAP,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_Bcast(ap%ds,NRAP,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

end subroutine mpibcastap

subroutine mpiscat(buf,ndx,n,add)
  integer, intent(in) :: ndx,n,add
  double precision, intent(inout) :: buf(*)

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
        counts(i) = ndx*np(i)+add
        displacements(i) = ndx*loop_start
        loop_start = loop_start + np(i)
     enddo
     counts(1) = 0
  endif

  na0=np(iam+1)*ndx+add
  if(iam==0)na0=0
  call MPI_Scatterv(buf,counts,displacements,MPI_DOUBLE_PRECISION, &
       buf,na0,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  deallocate(np)
  if(iam==0)deallocate(counts,displacements)
end subroutine mpiscat

subroutine mpigat(buf,ndx,n)
  integer, intent(in) :: ndx,n
  double precision, intent(inout) :: buf(ndx,*)

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

subroutine mpiend()
  integer ierr

  call MPI_Bcast(AUTO_MPI_KILL_MESSAGE,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  call MPI_Finalize(ierr)
end subroutine mpiend

subroutine mpitim(tim)
  double precision tim
  tim = MPI_Wtime()
end subroutine mpitim  

end module autompi
