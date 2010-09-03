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

module autompi

use auto_constants, only: autoparameters, niap, nrap

implicit none
private

public :: mpiini, mpiiap, mpiwfi, mpicon, mpisbv, mpibcast, mpibcasti
public :: mpibcastap
public :: mpiscat, mpigat, mpiend, mpitim, mpiiam, mpikwt, partition

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
     m(i) = i*n/kwt - (i-1)*n/kwt
  enddo
end subroutine partition

subroutine mpicon(s1,a1,a2,bb,cc,d,faa,fc,ntst,nov,ncb,nrc,ifst)
  integer, intent(in) :: ntst, nov, ncb, nrc, ifst
  double precision, intent(inout) :: a1(nov,nov,*),a2(nov,nov,*),bb(ncb,nov,*)
  double precision, intent(inout) :: cc(nov,nrc,*)
  double precision, intent(inout) :: s1(nov,nov,*),d(ncb,*),faa(nov,*),fc(*)

  double precision, allocatable :: ccb(:,:,:)
  integer,allocatable :: np(:)
  integer :: i,ii,j,k,ierr,iam,kwt
  logical ia1(ntst),ia2(ntst),is1(ntst),icc(ntst+1)

  call MPI_Comm_rank(MPI_COMM_WORLD,iam,ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,kwt,ierr)
  allocate(np(kwt))
  call partition(ntst,kwt,np)

  ! Worker is running now

  ia1=.false.
  ia2=.false.
  is1=.false.
  icc=.false.
  call reduceidx(1,ntst,ntst,kwt,ia1,ia2,is1,icc)

  call mpigatidx(ia2,faa,1,nov,np,ntst,iam)
  call mpisum(fc,nrc)

  if(ifst==0)then
     deallocate(np)
     return
  endif

  call mpigatidx(is1,s1,nov,nov,np,ntst,iam)
  call mpigatidx(ia1,a1,nov,nov,np,ntst,iam)
  call mpigatidx(ia2,a2,nov,nov,np,ntst,iam)
  call mpigatidx(ia2,bb,ncb,nov,np,ntst,iam)

  allocate(ccb(nov,nrc,kwt))
  call MPI_Gather(cc(1,1,np(iam+1)+1),nov*nrc,MPI_DOUBLE_PRECISION, &
       ccb,nov*nrc,MPI_DOUBLE_PRECISION, &
       0,MPI_COMM_WORLD,ierr)
  call mpigatidx(icc,cc,nov,nrc,np,ntst,iam)

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

!-------- ---------- ---------
recursive subroutine reduceidx(lo,hi,ntst,kwt,ia1,ia2,is1,icc)

! Arguments
  integer lo,hi,ntst,kwt
  logical ia1(*),ia2(*),is1(*),icc(*)

! Local 
  integer mid

! This is a check for the master reduction so it will stop as soon
! as there is no more overlap (already handled by workers).
  if((lo*kwt-1)/ntst.eq.(hi*kwt-1)/ntst)return

! Obtain indices of matrices used for reduce in the master
! so we know which parts to send to it.

  mid=(lo+hi)/2

  if(lo<mid) &
       call reduceidx(lo,mid,ntst,kwt,ia1,ia2,is1,icc)

  if(mid+1<hi) &
       call reduceidx(mid+1,hi,ntst,kwt,ia1,ia2,is1,icc)

  if(lo==mid)then
     ia1(mid)=.true.
  else
     is1(mid)=.true.
  endif
  if(mid+1<hi)then
     is1(hi)=.true.
  else
     ia1(mid+1)=.true.
  endif
  ia2(mid)=.true.
  ia2(hi)=.true.
  icc(lo)=.true.
  icc(mid+1)=.true.
  icc(hi+1)=.true.

end subroutine reduceidx

subroutine mpigatidx(idx,a,nc,nr,np,ntst,iam)

  ! like mpigat() but only sends/receives blocks i for which
  ! idx(i) is .true.
  logical idx(*)
  integer nc, nr, np(*), ntst, iam
  double precision a(nc,nr,*)

  integer base, i, ii, j, ierr
  integer status(MPI_STATUS_SIZE)

  base=np(1)
  ii=base+np(2)
  j=2
  do i=np(1)+1,ntst
     if(idx(i))then
        if(iam==0)then
           call MPI_Recv(a(1,1,i),nc*nr,MPI_DOUBLE_PRECISION,j-1, &
                MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
        else if(iam==j-1)then
           call MPI_Send(a(1,1,i-base),nc*nr,MPI_DOUBLE_PRECISION,0, &
                0, MPI_COMM_WORLD, ierr)
        endif
     endif
     if(i==ii)then
        j=j+1
        base=ii
        ii=ii+np(j)
     endif
  enddo

end subroutine mpigatidx

subroutine mpisbv(ap,par,icp,ndim,ups,uoldps,udotps,upoldp,dtm, &
     thu,ifst,nllv)

  type(autoparameters) :: ap
  integer, intent(in) :: ndim,icp(*)
  integer, intent(inout) :: ifst,nllv
  double precision :: par(*),dtm(*),thu(*)
  double precision :: ups(ndim,0:*),uoldps(ndim,0:*),udotps(ndim,0:*),upoldp(ndim,0:*)

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
  call MPI_Pack_size(npar+ndim*8, &
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
     call MPI_Pack(icp    ,nfpr+nint,MPI_INTEGER,buffer,bufsize,pos, &
          MPI_COMM_WORLD,ierr)

     call MPI_Pack(thu    ,ndim*8,MPI_DOUBLE_PRECISION,buffer,bufsize,pos, &
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
     call MPI_Unpack(buffer,bufsize,pos,icp   ,nfpr+nint, &
          MPI_INTEGER,MPI_COMM_WORLD,ierr)
     call MPI_Unpack(buffer,bufsize,pos,thu   ,ndim*8, &
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

subroutine mpisum(buf,len)
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
  integer ierr

  call MPI_Bcast(AUTO_MPI_KILL_MESSAGE,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  call MPI_Finalize(ierr)
end subroutine mpiend

subroutine mpitim(tim)
  double precision tim
  tim = MPI_Wtime()
end subroutine mpitim  

end module autompi
