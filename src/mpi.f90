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
use mpi
!$ use omp_lib

implicit none
private

public :: mpiini, mpiiap, mpiwfi, mpireduce, mpibcksub, mpisbv, mpicbv
public :: mpibcast, mpibcast1, mpibcasti, mpibcast1i, mpibcast1l, mpibcastap
public :: mpireducemax, mpireducemin
public :: mpigat, mpiscat, mpiend, mpitim, mpiiam, mpikwt, partition
public :: mpigats

integer, parameter :: AUTO_MPI_KILL_MESSAGE = 0, AUTO_MPI_SOLVBV_MESSAGE = 1
integer, parameter :: AUTO_MPI_INIT_MESSAGE = 2

contains

subroutine mpiini()

  integer ierr,iam,namelen,provided
  character(len=MPI_MAX_PROCESSOR_NAME) processor_name
  integer :: message_type

  provided = MPI_THREAD_SINGLE
!$ if (omp_get_max_threads() > 1) then
!$   call MPI_Init_thread(MPI_THREAD_FUNNELED,provided,ierr)
!$ endif
  if (provided == MPI_THREAD_SINGLE) then
     ! if MPI lib doesn't officially support threads try anyway
     ! since we only call MPI when no other threads are active
     call MPI_Init(ierr)
  endif
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

logical function mpiwfi()
  integer :: message_type, ierr

  mpiwfi = .false.

  call MPI_Bcast(message_type,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  
  select case(message_type)
  case(AUTO_MPI_KILL_MESSAGE) ! The kill message
     call MPI_Finalize(ierr)
     stop
  case(AUTO_MPI_INIT_MESSAGE)
     ! do nothing
  case(AUTO_MPI_SOLVBV_MESSAGE) ! The solvbv message
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
     lo,hi,level)
  integer, intent(in) :: ntst,nov,ncb,nrc,ifst,nllv,lo,hi,level
  double precision, intent(inout) :: a1(nov,nov,*),a2(nov,nov,*),bb(ncb,nov,*)
  double precision, intent(inout) :: cc(nov,nrc,*),c2(nov,nrc,*)
  double precision, intent(inout) :: dd(ncb,nrc,*),faa(nov,*)
  double precision, intent(inout) :: fcfc(nrc,*)

  integer :: iam,kwt,mid,nlo,nmid1,hi1,mid1,base,na

  iam=mpiiam()
  kwt=mpikwt()
  mid=(lo+hi)/2
  nlo=(lo-1)*kwt/ntst
  nmid1=mid*kwt/ntst
  base=(iam*ntst+kwt-1)/kwt
  na=((iam+1)*ntst+kwt-1)/kwt-base
  mid1=mid-base
  if(mid1>na)then
     mid1=na+level+1
  endif
  hi1=hi-base
  if(hi1>na)then
     hi1=na+level
  endif
  if(nmid1==iam.and.nlo<iam)then
     if(nllv.eq.0)then
        call mpisend(faa(1,hi1),nov,nlo)
        call mpisend(fcfc(1,hi1),nrc,nlo)
     endif
     if(ifst.eq.1)then
        ! send hi to lo
        call mpisend(a1(1,1,mid1+1),nov*nov,nlo)
        call mpisend(a2(1,1,hi1),nov*nov,nlo)
        call mpisend(bb(1,1,hi1),nov*ncb,nlo)
        call mpisend(cc(1,1,mid1+1),nov*nrc,nlo)
        call mpisend(c2(1,1,hi1),nov*nrc,nlo)
        call mpisend(dd(1,1,hi1),ncb*nrc,nlo)
     endif
  elseif(nmid1>iam.and.nlo==iam)then
     if(nllv.eq.0)then
        call mpirecv(faa(1,hi1),nov,nmid1)
        call mpirecv(fcfc(1,hi1),nrc,nmid1)
     else
        faa(:,hi1)=0
        fcfc(:,hi1)=0
     endif
     if(ifst.eq.1)then
        ! receive hi
        call mpirecv(a1(1,1,mid1+1),nov*nov,nmid1)
        call mpirecv(a2(1,1,hi1),nov*nov,nmid1)
        call mpirecv(bb(1,1,hi1),nov*ncb,nmid1)
        call mpirecv(cc(1,1,mid1+1),nov*nrc,nmid1)
        call mpirecv(c2(1,1,hi1),nov*nrc,nmid1)
        call mpirecv(dd(1,1,hi1),ncb*nrc,nmid1)
     endif
  endif
end subroutine mpireduce

subroutine mpibcksub(sol,fc,ntst,nov,ncb,lo,hi,level)
  integer, intent(in) :: ntst,nov,ncb,lo,hi,level
  double precision, intent(inout) :: sol(nov,*),fc(*)
  integer :: iam,kwt,mid,nlo,nmid1,hi1,mid1,base,na

  iam=mpiiam()
  kwt=mpikwt()
  mid=(lo+hi)/2
  nlo=(lo-1)*kwt/ntst
  nmid1=mid*kwt/ntst
  base=(iam*ntst+kwt-1)/kwt
  na=((iam+1)*ntst+kwt-1)/kwt-base
  mid1=mid-base
  if(mid1>na)then
     mid1=na+level+1
  endif
  hi1=hi-base
  if(hi1>na)then
     hi1=na+level
  endif
  if(nmid1==iam.and.nlo<iam)then
     call mpirecv(sol(1,mid1+1),nov,nlo)
     call mpirecv(sol(1,hi1+1),nov,nlo)
     call mpirecv(fc,nov+ncb,nlo)
  elseif(nmid1>iam.and.nlo==iam)then
     call mpisend(sol(1,mid1+1),nov,nmid1)
     call mpisend(sol(1,hi1+1),nov,nmid1)
     call mpisend(fc,nov+ncb,nmid1)
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

subroutine mpisbv(solvbv)

  logical, intent(in) :: solvbv

  integer :: ierr
  integer :: message

  ! Send message to get worker into solvbv/init cnrlbv mode
  if(solvbv)then
     message=AUTO_MPI_SOLVBV_MESSAGE
  else
     message=AUTO_MPI_INIT_MESSAGE
  endif
  call MPI_Bcast(message,1,MPI_INTEGER,0, &
       MPI_COMM_WORLD,ierr)

end subroutine mpisbv

subroutine mpicbv(npar,par,rds)

  integer, intent(in) :: npar
  double precision :: par(npar)
  double precision :: rds

  integer :: iam,bufsize
  double precision, allocatable :: buffer(:)

  iam=mpiiam()
  bufsize = npar+1
  allocate(buffer(bufsize))

  if(iam==0)then
     buffer(1:npar)=par(:)
     buffer(npar+1)=rds
  endif

  call mpibcast(buffer,bufsize)

  if(iam>0)then
     par(1:npar)=buffer(1:npar)
     rds=buffer(npar+1)
  endif

  deallocate(buffer)

  ! Worker runs here

end subroutine mpicbv

subroutine mpibcast(buf,len)
  integer, intent(in) :: len
  double precision, intent(inout) :: buf(len)

  integer :: ierr

  call MPI_Bcast(buf,len,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
end subroutine mpibcast

subroutine mpibcast1(buf)
  double precision, intent(inout) :: buf

  integer :: ierr

  call MPI_Bcast(buf,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
end subroutine mpibcast1

subroutine mpibcasti(buf,len)
  integer, intent(in) :: len
  integer, intent(inout) :: buf(len)

  integer :: ierr

  call MPI_Bcast(buf,len,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
end subroutine mpibcasti

subroutine mpibcast1i(buf)
  integer, intent(inout) :: buf

  integer :: ierr

  call MPI_Bcast(buf,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
end subroutine mpibcast1i

subroutine mpibcast1l(buf)
  logical, intent(inout) :: buf

  integer :: ierr

  call MPI_Bcast(buf,1,MPI_LOGICAL,0,MPI_COMM_WORLD,ierr)
end subroutine mpibcast1l

subroutine mpibcastap(ap)
  type(autoparameters), intent(inout) :: ap

  integer :: ierr

  call MPI_Bcast(ap%ndim,NIAP,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_Bcast(ap%ds,NRAP,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

end subroutine mpibcastap

subroutine mpicounts(np,kwt,ndx,add,counts,displacements)
  integer, intent(in) :: np(kwt), kwt, ndx, add
  integer, intent(out) :: counts(kwt), displacements(kwt)
  
  integer i, loop_start

  loop_start = 0
  do i=1,kwt
     counts(i) = ndx*np(i)+add
     displacements(i) = ndx*loop_start
     loop_start = loop_start + np(i)
  enddo

end subroutine mpicounts

subroutine mpiscat(buf,ndx,n,add)
  integer, intent(in) :: ndx,n,add
  double precision, intent(inout) :: buf(*)

  integer, allocatable :: counts(:), displacements(:), np(:)
  integer ierr, iam, kwt, na0

  call MPI_Comm_size(MPI_COMM_WORLD,kwt,ierr)
  allocate(np(kwt))
  call partition(n,kwt,np)

  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  if(iam==0)then
     allocate(counts(kwt),displacements(kwt))
     call mpicounts(np,kwt,ndx,add,counts,displacements)
     call MPI_Scatterv(buf,counts,displacements,MPI_DOUBLE_PRECISION, &
          MPI_IN_PLACE,0,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
     deallocate(counts,displacements)
  else
     na0=np(iam+1)*ndx+add
     call MPI_Scatterv(buf,counts,displacements,MPI_DOUBLE_PRECISION, &
          buf,na0,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  endif

  deallocate(np)
end subroutine mpiscat

subroutine mpigat(buf,ndx,n)
  integer, intent(in) :: ndx,n
  double precision, intent(inout) :: buf(ndx,*)

  integer, allocatable :: counts(:), displacements(:), np(:)
  integer ierr, iam, kwt, na0

  call MPI_Comm_size(MPI_COMM_WORLD,kwt,ierr)
  allocate(np(kwt))
  call partition(n,kwt,np)

  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  if(iam==0)then
     allocate(counts(kwt),displacements(kwt))
     call mpicounts(np,kwt,ndx,0,counts,displacements)
     call MPI_Gatherv(MPI_IN_PLACE,0,MPI_DOUBLE_PRECISION, &
          buf,counts,displacements,MPI_DOUBLE_PRECISION, &
          0,MPI_COMM_WORLD,ierr)
     deallocate(counts,displacements)
  else
     na0=np(iam+1)*ndx
     call MPI_Gatherv(buf,na0,MPI_DOUBLE_PRECISION, &
          buf,counts,displacements,MPI_DOUBLE_PRECISION, &
          0,MPI_COMM_WORLD,ierr)
  endif

  deallocate(np)
end subroutine mpigat

subroutine mpigats(buf,ndx,n,linelen)
  ! gather line strings from each worker for output to fort.8/s.*
  integer, intent(in) :: ndx,n,linelen
  character(linelen), intent(inout) :: buf(*)

  integer, allocatable :: counts(:), displacements(:), np(:)
  integer ierr, iam, kwt, na0, line

  call MPI_Comm_size(MPI_COMM_WORLD,kwt,ierr)
  allocate(np(kwt))
  call partition(n,kwt,np)

  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  call MPI_Type_contiguous(linelen,MPI_CHARACTER,line,ierr)
  call MPI_Type_commit(line,ierr)
  if(iam==0)then
     allocate(counts(kwt),displacements(kwt))
     call mpicounts(np,kwt,ndx,0,counts,displacements)
     call MPI_Gatherv(MPI_IN_PLACE,0,line,buf,counts,displacements, &
          line,0,MPI_COMM_WORLD,ierr)
     deallocate(counts,displacements)
  else
     na0=np(iam+1)*ndx
     call MPI_Gatherv(buf,na0,line,buf,counts,displacements, &
          line,0,MPI_COMM_WORLD,ierr)
  endif
  call MPI_Type_free(line,ierr)

  deallocate(np)
end subroutine mpigats

subroutine mpireducemax(buf,n)
  integer, intent(in) :: n
  double precision, intent(inout) :: buf(n)

  integer ierr

  if (mpiiam()==0)then
     call MPI_Reduce(MPI_IN_PLACE, buf, n, MPI_DOUBLE_PRECISION, &
          MPI_MAX, 0, MPI_COMM_WORLD, ierr)
  else
     call MPI_Reduce(buf, buf, n, MPI_DOUBLE_PRECISION, &
          MPI_MAX, 0, MPI_COMM_WORLD, ierr)
  endif
end subroutine mpireducemax

subroutine mpireducemin(buf,n)
  integer, intent(in) :: n
  double precision, intent(inout) :: buf(n)

  integer ierr

  if (mpiiam()==0)then
     call MPI_Reduce(MPI_IN_PLACE, buf, n, MPI_DOUBLE_PRECISION, &
          MPI_MIN, 0, MPI_COMM_WORLD, ierr)
  else
     call MPI_Reduce(buf, buf, n, MPI_DOUBLE_PRECISION, &
          MPI_MIN, 0, MPI_COMM_WORLD, ierr)
  endif
end subroutine mpireducemin

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
